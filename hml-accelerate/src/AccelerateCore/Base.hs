{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Lib
-- Copyright   : [2018] J X-Ray Ho
-- License     : BSD3
--
-- Maintainer  : J X-Ray Ho <jho.xray@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

{-
************ Backpropagation / Stochastic descent training algorithm ****************
taken from the excellent http://neuralnetworksanddeeplearning.com/chap2.html

We have a network with L layers.
0. Split training set into batches of m elements that we will use for averaging
1. For each batch of m elements:
2. For each training example Xi (where i = [0...m]) - set input activation Ai1 (first index - # of sample in a batch, second - # of layer)

    2.1 Feedforward: For each l=2,3,…,L compute Zil = Wl * Ai(l−1) + Bl and Ail=σ(Zil), where:
            - Zil - intermediary neurons output vector
            - Wl  - matrix of weights from the previous layer into this (l-th)
            - Bl  - vector of biases
            - Ail - vector of outputs of l-the layer after activation function
            - σ   - activation function (can it be different for different layers?)

    2.2 Output error: Compute the vector δiL=∇aCi ⊙ σ′(ZiL), where:
            - δiL is the error of the output layer
            - ∇aCi is the gradient vector of the loss function C on i-th sample in a batch
            - σ′(ZiL) - derivative of the activation function σ on the output layer Z values

    2.3 Backprop the error: For each l=L−1,L−2,…,2 compute δil=( (Wl+1)T * δi(l+1) ) ⊙ σ′(Zxl), where:
            - δil is the error of the l-th layer for i-th sample in a batch
            - (Wl+1)T is transposed matrix of weights for (l+1)-th layer
            - σ′(Zxl) - derivative of the activation function on the current (l-th) layer Z values

3. Once the batch of m inputs is done, do gradient descent: For each l=L,L−1,…,2: 
        update the weights according to the rule Wl → Wl − (η/m) * ∑ δil * (Ai(l−1))T 
        and the biases according to the rule     Bl → Bl − (η/m) * ∑ δil
        where summation is by i (0 to m), η is the learning rate and T is transpose operation (so sigma by A is a matrix).

NB: for gradient descent step, only errors δil and outputs Ail are used, which means we either have to store them explicitly, (NOT good memory wise)
    or store running total of δil * (Ai(l−1))T and δil, which will allow us to calculate mean once the batch is over. 
    
    This approach is clearly preferred, so we will modify the original algorithm after step 2.3 as follows:

    2.4 For the current (i-th) run in a batch, calculate 
                WERRl → WERRl + δil * (Ai(l−1))T
                BERRl → BERRl + δil
            WERRl and BERRl need to be initialized to 0 matrices in the beginning of every batch.

3. Once the batch of m inputs is done, do gradient descent: For each l=L,L−1,…,2: 
        update the weights according to the rule Wl → Wl − (η/m) * WERRl 
        and the biases according to the rule     Bl → Bl − (η/m) * BERRl
        where summation is by i (0 to m), η is the learning rate and T is transpose operation (so sigma by A is a scalar).



From the practical point of view since we want to use Accelerate lib here, which basically operates in Tensor logic,
for efficiency we need to store most of these parameters in certain tensors, namely:

assuming network of L layers and training batch sized M:

weights :: Z :. L :. Ni :. No - in effect, it's an array of size L (# of layers) of Ni x No matrices of weights. However,
                                we won't be able to represent it this way, as Ni and No will be different from network to network.
                                We can use the Segment trick Accelerate uses for 1-dimensional arrays, but might be trickier here.
                                Alternative is simply a list of Ni x No arrays - which we may not be able to pass to the computation...
                                
                                TODO!!!!! -- just use vectors and complicated shape descriptor (T.1)
                                
                                Another alternative is to use tuples of arrays, but it is currently limited to 15 in accelerate - 
                                so we won't be able to model more than 15 layers (if they are of variable size, there's no such problem 
                                    for same sized layers).
                                Yet another alternative - represent the network as a FUNCTION of array terms Acc a -> Acc b etc,
                                where network structure will be embedded in this representation. This might be too difficult.
                                It looks like (T.1) is the only feasible way.

z :: Z :. L :. Int  - in effect, array of size L of vectors of Int

-}

module AccelerateCore.Base where

import System.IO
import Data.Array.Accelerate as A
import Data.Array.Accelerate.System.Random.MWC
import Data.Array.Accelerate.Numeric.LinearAlgebra

-- let's try implementing algo above in a bottom-up way, from smallest functions

type NNElement = Float -- synonim for whether we want to operate on single or double precision

vecplus :: Acc (Vector NNElement) -> Acc (Vector NNElement) -> Acc (Vector NNElement)
vecplus a b = A.zipWith (+) a b

hadamart :: Acc (Vector NNElement) -> Acc (Vector NNElement) -> Acc (Vector NNElement)
hadamart a b = A.zipWith (*) a b

-- 1) compute Zil = Wl * Ai(l−1) + Bl and Ail=σ(Zil)
-- no size checks are done - it MUST be done at network construction step
computeZ :: Acc (Matrix NNElement) -> Acc (Vector NNElement) -> Acc (Vector NNElement) -> Acc (Vector NNElement)
computeZ w a b = w #> a `vecplus` b

-- activation function and its' differential as per "A Functional Approach To Neural Networks"
data ActivationSpec = ActivationSpec {
    asF :: Double -> Double,
    asF' :: Double -> Double,
    desc :: String
}

identityAS = ActivationSpec {
    asF = id,
    asF' = const 1,
    desc = "identity"
}

sigmoidAS = ActivationSpec {
    asF = \x -> 1 / (1 + exp (-1 * x) ),
    asF' = sigmoid',
    desc = "sigmoid" 
}

sigmoid' x = sg * (1 - sg) where sg = 1 / (1 + exp (-1 * x) ) 



