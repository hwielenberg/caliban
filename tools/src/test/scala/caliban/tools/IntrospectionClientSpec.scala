package caliban.tools

import caliban._
import caliban.quick._
import caliban.schema.Annotations._
import caliban.schema.Schema.auto._
import caliban.schema.ArgBuilder.auto._
import zio.test._
import zio.{ durationInt, Clock }

object IntrospectionClientSpec extends ZIOSpecDefault {

  case class MyObject(
    @GQLDeprecated("dep required") name: String,
    @GQLDeprecated("dep optional") length: Option[Int],
    otherObject:OtherObject
  )

  @GQLDeprecated("dep type")
  case class OtherObject(
                          name: String,
                        )

  case class Args(@GQLDeprecated("Use nameV2") name: Option[String] = Some("defaultValue"), nameV2: String)

  case class Queries(
    getObject: Args => MyObject
  )

  object Resolvers {
    def getObject(@GQLDeprecated("foobar") args: Args): MyObject =
      MyObject(
        args.name.getOrElse(""),
        Some(3),
        OtherObject("")
      )
  }

  val queries = Queries(
    getObject = Resolvers.getObject
  )

  val api = graphQL(
    RootResolver(
      queries
    )
  )

  def spec = suite("IntrospectionClientSpec")(
    test("is isomorphic") {
      for {
        _                 <- api
                               .runServer(
                                 port = 8087,
                                 apiPath = "/api/graphql"
                               )
                               .fork
        _                 <- Clock.ClockLive.sleep(2.seconds)
        introspectedSchema = SchemaLoader.fromIntrospection("http://localhost:8087/api/graphql", None)
        codeSchema         = SchemaLoader.fromCaliban(api)
        res               <- SchemaComparison.compare(
                               introspectedSchema,
                               codeSchema
                             )
      } yield assertTrue(res.isEmpty, false)
    }
  )

}
