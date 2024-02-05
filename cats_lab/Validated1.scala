package cats_labs

import cats.data.{Validated, ValidatedNec}

object Validated1 extends App {
  final case class RegistrationData(username: String, password: String, firstName: String, lastName: String, age: Int)

  sealed trait DomainValidation {
    def errorMessage: String
  }

  case object UsernameHasSpecialCharacters extends DomainValidation {
    def errorMessage: String = "Username cannot contain special characters."
  }

  case object PasswordDoesNotMeetCriteria extends DomainValidation {
    def errorMessage: String = "Password must be at least 10 characters long, including an uppercase and a lowercase letter, one number and one special character."
  }

  case object FirstNameHasSpecialCharacters extends DomainValidation {
    def errorMessage: String = "First name cannot contain spaces, numbers or special characters."
  }

  case object LastNameHasSpecialCharacters extends DomainValidation {
    def errorMessage: String = "Last name cannot contain spaces, numbers or special characters."
  }

  case object AgeIsInvalid extends DomainValidation {
    def errorMessage: String = "You must be aged 18 and not older than 75 to use our services."
  }

  import cats.syntax.all._

  sealed trait FormValidator {
    def validateUserName(userName: String): Either[DomainValidation, String] =
      Either.cond(
        userName.matches("^[a-zA-Z0-9]+$"),
        userName,
        UsernameHasSpecialCharacters
      )

    def validatePassword(password: String): Either[DomainValidation, String] =
      Either.cond(
        password.matches("(?=^.{10,}$)((?=.*\\d)|(?=.*\\W+))(?![.\\n])(?=.*[A-Z])(?=.*[a-z]).*$"),
        password,
        PasswordDoesNotMeetCriteria
      )

    def validateFirstName(firstName: String): Either[DomainValidation, String] =
      Either.cond(
        firstName.matches("^[a-zA-Z]+$"),
        firstName,
        FirstNameHasSpecialCharacters
      )

    def validateLastName(lastName: String): Either[DomainValidation, String] =
      Either.cond(
        lastName.matches("^[a-zA-Z]+$"),
        lastName,
        LastNameHasSpecialCharacters
      )

    def validateAge(age: Int): Either[DomainValidation, Int] =
      Either.cond(
        age >= 18 && age <= 75,
        age,
        AgeIsInvalid
      )

    def validateForm(username: String, password: String, firstName: String, lastName: String, age: Int): Either[DomainValidation, RegistrationData] = {

      for {
        validatedUserName <- validateUserName(username)
        validatedPassword <- validatePassword(password)
        validatedFirstName <- validateFirstName(firstName)
        validatedLastName <- validateLastName(lastName)
        validatedAge <- validateAge(age)
      } yield RegistrationData(validatedUserName, validatedPassword, validatedFirstName, validatedLastName, validatedAge)
    }

  }

  object FormValidator extends FormValidator

  val res = FormValidator.validateForm(
    username = "fakeUs3rname",
    password = "password",
    firstName = "John",
    lastName = "Doe",
    age = 15
  )

  res match {
    case Right(_) => println("all good")
    case Left(e) => println(s"exception: ${e.errorMessage}")
  }

  trait FormValidator2 {

    import cats.data._
    import cats.data.Validated._
    import cats.syntax.all._

    def validateUserName(userName: String): Validated[DomainValidation, String] = FormValidator.validateUserName(userName).toValidated

    def validatePassword(password: String): Validated[DomainValidation, String] = FormValidator.validatePassword(password).toValidated

    def validateFirstName(firstName: String): Validated[DomainValidation, String] = FormValidator.validateFirstName(firstName).toValidated

    def validateLastName(lastName: String): Validated[DomainValidation, String] = FormValidator.validateLastName(lastName).toValidated

    def validateAge(age: Int): Validated[DomainValidation, Int] = FormValidator.validateAge(age).toValidated
  }

  object FormValidator2 extends FormValidator2

  import FormValidator2._
  import cats.data.Validated._
  import cats.syntax.flatMap._

  /*def validateForm(username: String, password: String, firstName: String, lastName: String, age: Int): Validated[DomainValidation, RegistrationData] = {
   for {
      //CTE: value flatMap is not a member of cats.data.Validated[cats_labs.Validated1.DomainValidation,String]
      validatedUserName <- validateUserName(username)
      validatedPassword <- validatePassword(password)
      validatedFirstName <- validateFirstName(firstName)
      validatedLastName <- validateLastName(lastName)
      validatedAge <- validateAge(age)
    } yield RegistrationData(validatedUserName, validatedPassword, validatedFirstName, validatedLastName, validatedAge)
  }*/
  type ValidationResult[A] = ValidatedNec[DomainValidation, A]

  sealed trait FormValidatorNec {


    private def validateUserName(userName: String): ValidationResult[String] =
      if (userName.matches("^[a-zA-Z0-9]+$")) userName.validNec else UsernameHasSpecialCharacters.invalidNec

    private def validatePassword(password: String): ValidationResult[String] =
      if (password.matches("(?=^.{10,}$)((?=.*\\d)|(?=.*\\W+))(?![.\\n])(?=.*[A-Z])(?=.*[a-z]).*$")) password.validNec
      else PasswordDoesNotMeetCriteria.invalidNec

    private def validateFirstName(firstName: String): ValidationResult[String] =
      if (firstName.matches("^[a-zA-Z]+$")) firstName.validNec else FirstNameHasSpecialCharacters.invalidNec

    private def validateLastName(lastName: String): ValidationResult[String] =
      if (lastName.matches("^[a-zA-Z]+$")) lastName.validNec else LastNameHasSpecialCharacters.invalidNec

    private def validateAge(age: Int): ValidationResult[Int] =
      if (age >= 18 && age <= 75) age.validNec else AgeIsInvalid.invalidNec

    def validateForm(username: String, password: String, firstName: String, lastName: String, age: Int): ValidationResult[RegistrationData] = {
      (validateUserName(username),
        validatePassword(password),
        validateFirstName(firstName),
        validateLastName(lastName),
        validateAge(age)).mapN(RegistrationData)
    }

  }

  object FormValidatorNec extends FormValidatorNec

  val res2: ValidationResult[RegistrationData] = FormValidatorNec.validateForm(
    username = "fakeUs3rname",
    password = "password",
    firstName = "John",
    lastName = "Doe",
    age = 11
  )

  println("=====")
  res2 match {
    case Valid(_) => println("all good")
    case Invalid(errors) => errors.toList.foreach(error => println(s"- $error"))
  }
}
