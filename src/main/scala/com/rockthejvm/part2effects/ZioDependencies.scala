package com.rockthejvm.part2effects

import zio.*

object ZioDependencies extends ZIOAppDefault {
  import domain.*
//
//  val subscriptionService = ZIO.succeed(
//    UserSubscription.create(
//      EmailService.create(),
//      UserDatabase.create(
//        ConnectionPool.create(10)
//      )
//    )
//  )

  /*
    "clean DI" has drawbacks
      - does not scale for many services
      - DI can be 100x worse
        - pass dependencies partially
        - not having all deps in the same place
        - passing dependencies multiple times
   */

//  def subscribe(user: User) =
//    for
//      sub <- subscriptionService // service is instantiated at the point of call
//      _   <- sub.subscribeUser(user)
//    yield ()

  // risk leaking resources if you subscribe multiple users in the same program
//
//  val program =
//    for
//      _ <- subscribe(User("John Doe", "john.doe@unknown.com"))
//      _ <- subscribe(User("Jane Doe", "jane.doe@unknown.com"))
//    yield ()

  // alternative:
  def subscribe_v2(user: User) =
    for
      sub <- ZIO.service[UserSubscription] // ZIO[UserSubscription, Nothing, UserSubscription]
      _   <- sub.subscribeUser(user)
    yield ()

  val program_v2 =
    for
      _ <- subscribe_v2(User("John Doe", "john.doe@unknown.com"))
      _ <- subscribe_v2(User("Jane Doe", "jane.doe@unknown.com"))
    yield ()

  val subscriptionLayer = ZLayer.succeed(
    UserSubscription.create(
      EmailService.create(),
      UserDatabase.create(ConnectionPool.create(10))
    )
  )

  def run = program_v2.provide(
    subscriptionLayer
  )
}
