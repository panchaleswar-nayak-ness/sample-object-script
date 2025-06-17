Class Lab.Instrument.UI
{
    ClassMethod ShowStatus()
    {
        Set conn = ##class(Lab.Instrument.Connection).%New()
        Write "Connection Status: ", conn.GetStatus(), !
    }

    ClassMethod Connect()
    {
        Set conn = ##class(Lab.Instrument.Connection).%New()
        Do conn.ManualConnect()
        Write "Manual connect requested.", !
    }

    ClassMethod Disconnect()
    {
        Set conn = ##class(Lab.Instrument.Connection).%New()
        Do conn.ManualDisconnect()
        Write "Manual disconnect requested.", !
    }

    ClassMethod ShowLog()
    {
        Set i = ""
        For  Set i = $Order(^LabInstrumentLog(i)) Quit:i=""  {
            Write ^LabInstrumentLog(i), !
        }
    }
}
