Class Lab.Instrument.Config Extends %Persistent
{
    Property IPAddress As %String;
    Property Port As %Integer;
    Property RetryCount As %Integer;
    Property RetryInterval As %Integer; // seconds

    ClassMethod GetConfig() As %RegisteredObject
    {
        Set config = ..%OpenId(1)
        If '$IsObject(config) {
            Set config = ##class(Lab.Instrument.Config).%New()
            Set config.IPAddress = "127.0.0.1"
            Set config.Port = 12345
            Set config.RetryCount = 3
            Set config.RetryInterval = 5
            Do config.%Save()
        }
        Quit config
    }
}

