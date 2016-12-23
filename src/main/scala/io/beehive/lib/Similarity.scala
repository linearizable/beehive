package io.beehive.lib

import scala.collection.immutable.ListMap
import io.beehive.actors.ItemActor._

trait Similarity {
    def similarity : List[SimilarResult]
}

class Cooccurrence(similarItems: Map[String, Int], 
                   numItems: Int) extends Similarity {
    def similarity() = {
        /**
         * Simply sort items by num occurrences together and 
         * return top numItems
         */ 
        similarItems.toSeq.sortBy(-_._2).map(x => SimilarResult(x._1, x._2.toDouble)).toList.take(numItems)
    }
}

class Jaccard(similarItems: Map[String, Int], 
              occurrences: Int, 
              similarItemOccurrences: Map[String, Int], 
              numItems: Int) extends Similarity {
    def similarity() = {
        /**
         * Compute jaccard score for each item
         */
        val similarItemsWithJaccardScore = similarItems.map { case(itemId, occurrencesTogether) => {
            itemId -> computeScore(itemId, occurrencesTogether)
        }}
        
        /**
         * Sort by jaccard score and return top numItems
         */ 
        similarItemsWithJaccardScore.toSeq.sortBy(-_._2).map(SimilarResult(_)).toList.take(numItems)
    }
    
    /**
     * Compute jaccard score
     * = A intersection B / A union B
     */ 
    def computeScore(itemId: String, occurrencesTogether: Int) = {
        occurrencesTogether.toDouble/(occurrences+similarItemOccurrences(itemId)-occurrencesTogether)
    }
}

class Cosine(similarItems: Map[String, Int], 
             occurrences: Int, 
             similarItemOccurrences: Map[String, Int], 
             numItems: Int) extends Similarity {
    def similarity() = {
        /**
         * Compute cosine score for each item
         */
        val similarItemsWithCosineScore = similarItems.map { case(itemId, occurrencesTogether) => {
            itemId -> computeScore(itemId, occurrencesTogether)
        }}
        
        /**
         * Sort by cosine score and return top numItems
         */ 
        similarItemsWithCosineScore.toSeq.sortBy(-_._2).map(SimilarResult(_)).toList.take(numItems)
    }
    
    /**
     * Compute cosine score
     */ 
    def computeScore(itemId: String, occurrencesTogether: Int) = {
        occurrencesTogether.toDouble/(math.sqrt(occurrences) * math.sqrt(similarItemOccurrences(itemId)))
    }
}

class LogLikelihood(similarItems: Map[String, Int], 
                    occurrences: Int, 
                    similarItemOccurrences: Map[String, Int], 
                    numUsers: Int,
                    numItems: Int) extends Similarity {
    def similarity() = {
        /**
         * Compute log likelihood score for each item
         */
        val similarItemsWithLLRScore = similarItems.map { case(itemId, occurrencesTogether) => {
            itemId -> computeScore(itemId, occurrencesTogether)
        }}
        
        /**
         * Sort by log likelihood score and return top numItems
         */ 
        similarItemsWithLLRScore.toSeq.sortBy(-_._2).map(SimilarResult(_)).toList.take(numItems    ) 
    }
    
    /**
     * Compute loglikelihood score
     * item1 -> For which we are computing score
     * item2 -> Against which we are computing score
     * e.g. we are computing LL score for (item1 -> item2)
     */ 
    def computeScore(itemId: String, occurrencesTogether: Int) = {
        val item2 = itemId
        
        /**
         * Individual occurrences of item1 and item2
         */ 
        val item1Occurrences = occurrences
        val item2Occurrences = similarItemOccurrences(item2)
        
        /**
         * Occurrences of item1 and item2 without each other
         * e.g. no. of users who interacted with item1 and not item2 
         * and vice-versa
         * Also called k12 and k21
         */ 
        val item1WithoutItem2 = item1Occurrences - occurrencesTogether
        val item2WithoutItem1 = item2Occurrences - occurrencesTogether
        
        /**
         * Number of users who interacted with neither item1 nor item2
         * Also called k22
         */ 
        val neitherOccurrences = numUsers - item1Occurrences - item2Occurrences + occurrencesTogether
        
        /**
         * For the LLR matrix. occurrencesTogether is k11
         * k11 | k21
         * k12 | k22
         * and compute entropies
         */ 
        val rowEntropy = entropy(Seq(occurrencesTogether + item2WithoutItem1, item1WithoutItem2 + neitherOccurrences))
        val columnEntropy = entropy(Seq(occurrencesTogether + item1WithoutItem2, item2WithoutItem1 + neitherOccurrences))
        val matrixEntropy = entropy(Seq(occurrencesTogether, item2WithoutItem1, item1WithoutItem2, neitherOccurrences))
        
        /**
         * Compute final LLR score
         */ 
        if(rowEntropy + columnEntropy < matrixEntropy) {
            0.0
        }
        else {
            val LLScore = 2 * (rowEntropy + columnEntropy - matrixEntropy)
            1 - 1/(1+LLScore)
        }
    }
    
    def entropy(elements: Seq[Int]) : Double = {
        val total = elements.sum
        var result:Double = elements.foldLeft(0.0)(_+xLogx(_))
        xLogx(total) - result
    }

    def xLogx(value: Int) = {
        if(value == 0) 0 else value * math.log(value)
    }
}
