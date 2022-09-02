package com.rockthejvm.part2effects

import zio.*

import java.util.concurrent.TimeUnit

object ZioDependencies extends ZIOAppDefault:
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

  val subscriptionLayer =
    ZLayer.succeed(
      UserSubscription.create(
        EmailService.create(),
        UserDatabase.create(ConnectionPool.create(10))
      )
    )

  /** ZLayers
    */
  val connPool: ZLayer[Any, Nothing, ConnectionPool] =
    ZLayer.succeed(ConnectionPool.create(10))

  val dbLayer: ZLayer[ConnectionPool, Nothing, UserDatabase] =
    ZLayer.fromFunction(UserDatabase.create _)

  // compose layers
  val database: ULayer[UserDatabase] =
    connPool >>> dbLayer

  val emailServiceLayer: ULayer[EmailService] =
    ZLayer.succeed(EmailService.create())

  val databaseWithEmailService: ZLayer[Any, Nothing, UserDatabase & EmailService] =
    database ++ emailServiceLayer

  val userSubscriptionServiceLayer: ZLayer[UserDatabase & EmailService, Nothing, UserSubscription] =
    ZLayer.fromFunction(UserSubscription.create _)

  val userSubscription: ZLayer[Any, Nothing, UserSubscription] =
    databaseWithEmailService >>> userSubscriptionServiceLayer

  // magic v2 (to organize magical deps)
  val userSubscription_v2 = ZLayer.make[UserSubscription](
    UserSubscription.layer,
    EmailService.layer,
    UserDatabase.layer,
    ConnectionPool.layer(10)
  )

  // pass-through (pass dependency into result)
  val dbWithPoolLayer: ZLayer[ConnectionPool, Nothing, ConnectionPool & UserDatabase] =
    UserDatabase.layer.passthrough

  // service = take a dep and expose it as a value to further layer
  val dbService = ZLayer.service[UserDatabase]

  // launch = creates a ZIO that uses the services and never finishes
  // run a layer as a service
  val subscriptionLaunch = UserSubscription.layer.launch

  // memoization (active for default)
  val memoized = ZLayer.make[UserSubscription](
    UserSubscription.layer,
    EmailService.layer.fresh, // this one will always be new
    UserDatabase.layer,
    ConnectionPool.layer(10)
  )

  val getTime = Clock.currentTime(TimeUnit.SECONDS)

//  def run = program_v2.provide(
//    userSubscription
//  )
  val run = program_v2.provide(
    UserSubscription.layer,
    EmailService.layer,
    UserDatabase.layer,
    ConnectionPool.layer(10),
    ZLayer.Debug.tree
  )
