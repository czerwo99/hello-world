import org.atnos.eff._
import cats.syntax.all._
import cats.data._
import Eff._

object EffPlay extends App {

  import Effects._
  import WriterEffect._
  import ReaderEffect._
  import StateEffect._
  import org.atnos.eff.syntax.all._

  type RConfig[X] = Reader[AppConfig, X]
  type WString[X] = Writer[String, X]
  type SInt[X] = State[Int, X]

  type E = SInt |: RConfig |: WString |: NoEffect

  //Wont compile (with KindProjector)
  //Surprising because I'd thought the above type aliases were substitutable for these expressions
  //type E =  State[Int, ?] |: Reader[AppConfig, ?] |: Writer[String, ?] |: NoEffect

  println(App.startApp[E].runReader(AppConfig(7)).runWriter.runState(42).run)
}

case class AppConfig(threadCount: Int)

object App {

  import ReaderCreation._
  import WriterCreation._
  import StateCreation._

  type Env[E] = Member[Reader[AppConfig, ?], E]
  type Log[E] = Member[Writer[String, ?], E]
  type RNG[E] = Member[State[Int, ?], E]

  def startApp[E: Env: Log: RNG]: Eff[E, Unit] = for {
    c <- ask
    _ <- tell(s"starting ${c.threadCount} threads..")
    _ <- rollDice
  } yield ()

  def rollDice[E: RNG: Log]: Eff[E, Int] = for {
    seed <- get
    _ <- put(seed + 1)
    roll = new util.Random(seed).nextInt(6) + 1
    _ <- tell(s"rolled a dice: ${roll}")
  } yield roll

}
