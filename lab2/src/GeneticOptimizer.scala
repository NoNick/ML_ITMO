import scala.util.Random

object GeneticOptimizer extends Optimizer {
    val generationSize = 100
    val survivers = 10
    val mutateK = 0.5
    val generations = 50
    
    override def optimize(loss: Seq[Double] => Double, grad: Seq[Double] => Seq[Double],
                          params: Seq[Double]): Seq[Double] = {
        def cycle(generation: Seq[Seq[Double]]): Seq[Seq[Double]] = {
            val result = select(mutate(cross(generation)), loss)
            println("Cost: " + loss(result(0)))
            result
        }
        Function.chain(List.fill(generations)(cycle(_)))(List(params)).head
    }

    def mutate(generation: Seq[Seq[Double]]): Seq[Seq[Double]] = {
        def sumRand(x: Double): Double = x + (Random.nextDouble() * 2.0 - 1.0) * mutateK * x
        generation.map(_.map(sumRand))
    }

    def select(generation: Seq[Seq[Double]], loss: Seq[Double] => Double): Seq[Seq[Double]] =
        generation.sortBy(loss).take(survivers)

    def cross(generation: Seq[Seq[Double]]): Seq[Seq[Double]] = {
        def randSize(): Int = {
            val rand = math.abs(Random.nextInt()) % generation.size
            if (rand <= 1) 2 else rand
        }
        def crossSingle(): Seq[Double] = Random.shuffle(generation)
            .take(randSize()).transpose.map(xs => xs.sum / xs.length)
        Stream.from(0).take(generationSize).map(_ => crossSingle())
    }
}
