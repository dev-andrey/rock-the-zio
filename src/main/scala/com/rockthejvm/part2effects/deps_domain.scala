package com.rockthejvm.part2effects

import zio.*

object domain:
  // app to subscribe users to newsletter
  case class User(name: String, email: String)

  class UserSubscription(emailService: EmailService, userDatabase: UserDatabase):
    def subscribeUser(user: User): Task[Unit] =
      for
        _ <- emailService.email(user)
        _ <- userDatabase.insert(user)
      yield ()

  object UserSubscription:
    def create(emailService: EmailService, userDatabase: UserDatabase) =
      new UserSubscription(emailService, userDatabase)

  class EmailService:
    def email(user: User): Task[Unit] =
      ZIO.succeed(s"You've just been subscribed to Rock the JVM. Welcome, ${user.name}!").unit

  object EmailService:
    def create(): EmailService = new EmailService

  class UserDatabase(connectionPool: ConnectionPool):
    def insert(user: User): Task[Unit] =
      for
        conn <- connectionPool.get
        _    <- conn.runQuery(s"insert into subscribers(name,email) values (${user.name}, ${user.email})")
      yield ()

  object UserDatabase:
    def create(connectionPool: ConnectionPool) =
      new UserDatabase(connectionPool)

  class ConnectionPool(nrConnections: Int):
    def get: Task[Connection] =
      ZIO.log("Acquired connection") *> ZIO.succeed(Connection())

  object ConnectionPool:
    def create(nrConnections: Int) =
      new ConnectionPool(nrConnections)

  case class Connection():
    def runQuery(query: String): Task[Unit] =
      ZIO.log(s"Executing $query").unit
