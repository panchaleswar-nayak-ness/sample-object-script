Class Lab.Instrument.Connection Extends %RegisteredObject
{
    ClassMethod Connect()
    {
        Set config = ##class(Lab.Instrument.Config).GetConfig()
        Set retry = 0
        For  {
            Set sock = ##class(%Library.TCP).%New()
            Set sc = sock.Connect(config.IPAddress, config.Port)
            If sc=1 {
                Set ^LabConn("Socket") = sock
                Set ^LabConn("Status") = "Connected"
                Do ..Log("Connected to "_config.IPAddress_":"_config.Port)
                Quit
            } Else {
                Set retry = retry + 1
                Set ^LabConn("Status") = "Error: "_$System.Status.GetErrorText(sc)
                Do ..Log("Connection error: "_$System.Status.GetErrorText(sc))
                If retry'<config.RetryCount {
                    Do ##class(%SYS.Task).Sleep(config.RetryInterval)
                } Else {
                    Quit
                }
            }
        }
    }

    ClassMethod Disconnect()
    {
        If $IsObject(^LabConn("Socket")) {
            Do ^LabConn("Socket").Close()
            Set ^LabConn("Status") = "Disconnected"
            Do ..Log("Disconnected")
        }
    }

    ClassMethod GetStatus()
    {
        Quit ^LabConn("Status")
    }

    ClassMethod ManualConnect()
    {
        Do ..Connect()
    }

    ClassMethod ManualDisconnect()
    {
        Do ..Disconnect()
    }

    Method Log(msg As %String)
    {
        Set ^LabInstrumentLog($Increment(^LabInstrumentLog)) = $ZDATETIME($H,3)_" "_msg
    }

    Method HandleDisruption()
    {
        Set ^LabConn("Status") = "Connection lost. Attempting reconnect..."
        Do ..Log(^LabConn("Status"))
        Do ..Connect()
    }

    ClassMethod IsConnected()
    {
        Quit (^LabConn("Status") = "Connected")
    }
}
