  object Main extends App {

    import io.circe.magnolia.configured.Configuration
    import io.circe.parser.decode

      implicit val customConfig: Configuration = Configuration.default.withSnakeCaseMemberNames


    @derive(customizableDecoder, customizableEncoder)
    case class ParticipantEvent(
                                 oid: Option[EventOid],
                                 occurred: Option[java.time.LocalDate],
                                 status: Option[ParticipantVisitStatus]
                               )
    @derive(customizableDecoder, customizableEncoder)
    case class ParticipantScheduledEvent(
                                          events: List[ParticipantEvent]
                                        )
    // Sample JSON string
    val jsonString = """
                       |{
                       |	"schedule_uuid": "8a754d08-568e-4cc3-95b4-6f440eb0c8e3",
                       |	"events": [
                       |		{
                       |			"uuid": "4875a8a0-0ae3-4a6e-8717-7d7e1e6c4bbd",
                       |			"oid": "eventOid",
                       |			"targetDay": 0,
                       |			"occurred":"2024-03-18",
                       |			"study_environment_site_uuid": "83cc4d4c-b809-44ba-b535-7467832e1dca",
                       |			"status": "Pending",
                       |			"active": false
                       |		}
                       |	]
                       |}
                       |""".stripMargin

    // Parse JSON to case class using decode

    val result = decode[ParticipantScheduledEvent](jsonString)

    result match {
      case Right(myClass) => println(s"Parsing successful: $myClass")
      case Left(error) => println(s"Error parsing JSON: $error")
    }
  }
