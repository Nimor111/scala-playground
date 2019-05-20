package homework2

case class RegistrationForm(name: String,
                            email: String,
                            password: String,
                            passwordConfirmation: String,
                            birthYear: String,
                            birthMonth: String,
                            birthDay: String,
                            postalCode: String)

sealed trait RegistrationFormError

case object NameIsEmpty extends RegistrationFormError

case class InvalidEmail(email: String) extends RegistrationFormError

case object PasswordTooShort extends RegistrationFormError
case object PasswordRequiresGreaterSymbolVariety extends RegistrationFormError
case object PasswordsDoNotMatch extends RegistrationFormError

case class InvalidBirthdayDate(dateErrors: Chain[DateError])
    extends RegistrationFormError
case class BirthdayDateIsInTheFuture(date: Date) extends RegistrationFormError

case class InvalidPostalCode(code: String) extends RegistrationFormError

sealed trait DateError
case class YearIsNotAnInteger(year: String) extends DateError
case class MonthIsNotAnInteger(month: String) extends DateError
case class DayIsNotAnInteger(day: String) extends DateError
case class MonthOutOfRange(month: Int) extends DateError
case class DayOutOfRange(day: Int) extends DateError
case class InvalidDate(date: Date) extends DateError

case class Email(user: String, domain: String)

case class User(name: String,
                email: Email,
                passwordHash: String,
                birthday: Date,
                postalCode: Option[String])

object UserRegistration {
  def validateName(name: String): Validated[RegistrationFormError, String] = {
    name match {
      case "" => Invalid(Singleton(NameIsEmpty))
      case v  => Valid(v)
    }
  }

  def validateEmail(email: String): Validated[RegistrationFormError, Email] = {
    val hasAt = email.count(_ == '@') == 1
    val leftAndRight = email.split('@')

    if (!hasAt || leftAndRight.count(_ != "") != 2) {
      return Invalid(Singleton(InvalidEmail(email)))
    }

    Valid(Email(leftAndRight(0), leftAndRight(1)))
  }

  def validatePasswordLength(
      password: String): Validated[RegistrationFormError, String] = {
    if (password.length >= 8) Valid(password)
    else Invalid(Singleton(PasswordTooShort))
  }

  def validatePasswordSymbols(
      password: String): Validated[RegistrationFormError, String] = {
    if (password.count(!_.isLetterOrDigit) > 0 && password.count(_.isDigit) > 0 && password
          .count(_.isUpper) > 0) {
      Valid(password)
    } else Invalid(Singleton(PasswordRequiresGreaterSymbolVariety))
  }

  def validatePasswordConfirmation(password: String,
                                   passwordConfirmation: String)
    : Validated[RegistrationFormError, String] = {
    if (password == passwordConfirmation) Valid(passwordConfirmation)
    else Invalid(Singleton(PasswordsDoNotMatch))
  }

  def validatePassword(password: String, passwordConfirmation: String)
    : Validated[RegistrationFormError, String] = {

    (validatePasswordLength(password),
     validatePasswordSymbols(password),
     validatePasswordConfirmation(password, passwordConfirmation))
      .zipMap((p1: String, _: String, _: String) => p1)
  }

  def validateBirthYear(
      birthYear: String): Validated[RegistrationFormError, Int] = {
    if (birthYear == "" || !(birthYear forall Character.isDigit))
      Invalid(InvalidBirthdayDate(Singleton(YearIsNotAnInteger(birthYear))))
    else Valid(birthYear.toInt)
  }

  def validateBirthMonthInteger(
      birthMonth: String): Validated[RegistrationFormError, String] = {
    if (birthMonth == "" || !(birthMonth forall Character.isDigit))
      Invalid(InvalidBirthdayDate(Singleton(YearIsNotAnInteger(birthMonth))))
    else Valid(birthMonth)
  }

  def validateBirthMonthRange(
      birthMonth: String): Validated[RegistrationFormError, Int] = {
    val month = birthMonth.toInt
    if (month < 1 || month > 12)
      Invalid(InvalidBirthdayDate(Singleton(MonthOutOfRange(month))))
    else Valid(month)
  }

  def validateBirthDayInteger(
      birthDay: String): Validated[RegistrationFormError, String] = {
    if (birthDay == "" || !(birthDay forall Character.isDigit))
      Invalid(InvalidBirthdayDate(Singleton(YearIsNotAnInteger(birthDay))))
    else Valid(birthDay)
  }

  def validateBirthDayRange(
      birthDay: String): Validated[RegistrationFormError, Int] = {
    val day = birthDay.toInt
    if (day < 1 || day > 31)
      Invalid(InvalidBirthdayDate(Singleton(DayOutOfRange(day))))
    else Valid(day)
  }

  def validatePostalCode(
      verifier: String => Boolean,
      postalCode: String): Validated[RegistrationFormError, Option[String]] = {
    if (verifier(postalCode) || postalCode.isEmpty)
      Valid(Some(postalCode))
    else Invalid(Singleton(InvalidPostalCode(postalCode)))
  }

  def validateDate(birthYear: String,
                   birthMonth: String,
                   birthDay: String): Validated[RegistrationFormError, Date] = {
    for {
      birthYear <- validateBirthYear(birthYear)
      birthMonth <- validateBirthMonthInteger(birthMonth)
      birthMonthInt <- validateBirthMonthRange(birthMonth)
      birthDay <- validateBirthDayInteger(birthDay)
      birthDayInt <- validateBirthDayRange(birthDay)
      date <- validateDateFormat(Date(birthYear, birthMonthInt, birthDayInt))
    } yield date
  }

  def validateDateFormat(date: Date): Validated[RegistrationFormError, Date] = {
    import homework2.Validated.OptionToValidated
    Date
      .applyOption(date.year, date.month, date.day)
      .toValidated(
        InvalidBirthdayDate(
          Singleton(InvalidDate(Date(date.year, date.month, date.day)))))
  }

  def registerUser(userCountryPostalCodeVerifier: String => Boolean,
                   today: Date)(
      form: RegistrationForm): Validated[RegistrationFormError, User] = {
    (validateName(form.name),
     validateEmail(form.email),
     validatePassword(form.password, form.passwordConfirmation),
     validateDate(form.birthYear, form.birthMonth, form.birthDay),
     validatePostalCode(userCountryPostalCodeVerifier, form.postalCode))
      .zipMap(User.apply)
  }

}
