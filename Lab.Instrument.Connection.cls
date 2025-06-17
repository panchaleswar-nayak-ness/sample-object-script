Class Lab.Instrument.Connection Extends %RegisteredObject
{
    Property Status As %String [ Private ];
    Property Socket As %Stream.TCP [ Private ];
    Property Config As Lab.Instrument.Config [ Private ];

    Method %OnNew()
    {
        Set ..Config = ##class(Lab.Instrument.Config).GetConfig()
        Set ..Status = "Disconnected"
    }

    Method Connect() As %Status
    {
        Set config = ..Config
        Set retry = 0
        For  {
            Try {
                Set sock = ##class(%Stream.TCP).%New()
                Do sock.Connect(config.IPAddress, config.Port)
                Set ..Socket = sock
                Set ..Status = "Connected"
                Do ..Log("Connected to "_config.IPAddress_":"_config.Port)
                Quit $$$OK
            } Catch ex {
                Set retry = retry + 1
                Set ..Status = "Error: "_ex.DisplayString()
                Do ..Log("Connection error: "_ex.DisplayString())
                If retry'<config.RetryCount {
                    Do ##class(%SYS.Task).Sleep(config.RetryInterval)
                } Else {
                    Quit $$$ERROR
                }
            }
        }
    }

    Method Disconnect()
    {
        If $IsObject(..Socket) {
            Do ..Socket.Close()
            Set ..Status = "Disconnected"
            Do ..Log("Disconnected")
        }
    }

    Method GetStatus() As %String
    {
        Quit ..Status
    }

    Method ManualConnect()
    {
        Do ..Connect()
    }

    Method ManualDisconnect()
    {
        Do ..Disconnect()
    }

    Method Log(msg As %String)
    {
        // Simple logging to ^LabInstrumentLog global
        Set ^LabInstrumentLog($Increment(^LabInstrumentLog)) = $ZDATETIME($H,3)_" "_msg
    }

    Method HandleDisruption()
    {
        Set ..Status = "Connection lost. Attempting reconnect..."
        Do ..Log(..Status)
        Do ..Connect()
    }

    Method IsConnected() As %Boolean
    {
        Quit (..Status = "Connected")
    }
}
