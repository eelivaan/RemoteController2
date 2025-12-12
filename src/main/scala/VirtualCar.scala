
/**
 * Auton virtuaalinen malli, eli ns. digitaalinen kaksonen.
 * Olisi voitu laajentaa pitämään kirjaa auton sijainnista ja muista parametreista
 * sekä itsenäiseen ajamiseen liittyvistä asioista.
 */
object VirtualCar:

  var scanning: Boolean = true

  var curDriveCommand: Option[Char] = None

end VirtualCar
