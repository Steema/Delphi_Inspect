object DataModule1: TDataModule1
  Height = 480
  Width = 640
  object OrientationSensor1: TOrientationSensor
    Left = 58
    Top = 47
  end
  object MotionSensor1: TMotionSensor
    Left = 218
    Top = 23
  end
  object TetheringManager2: TTetheringManager
    Text = 'TetheringManager2'
    AllowedAdapters = 'Network'
    Left = 50
    Top = 103
  end
  object TetheringManager1: TTetheringManager
    Text = 'TetheringManager1'
    AllowedAdapters = 'Bluetooth'
    Left = 226
    Top = 87
  end
  object TetheringAppProfile1: TTetheringAppProfile
    Manager = TetheringManager1
    Text = 'TetheringAppProfile1'
    Actions = <>
    Resources = <>
    Left = 234
    Top = 159
  end
  object LocationSensor1: TLocationSensor
    ActivityType = Other
    UsageAuthorization = WhenInUse
    Left = 55
    Top = 184
  end
end
