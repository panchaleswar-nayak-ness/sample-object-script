Class Lab.Instrument.Config Extends %Persistent
{
    Property IPAddress As %String;
    Property Port As %Integer;
    Property RetryCount As %Integer [ InitialExpression = 3 ];
    Property RetryInterval As %Integer [ InitialExpression = 5 ]; // seconds

    ClassMethod GetConfig() As Lab.Instrument.Config
    {
        Set config = ..%OpenId(1)
        If '$IsObject(config) {
            Set config = ##class(Lab.Instrument.Config).%New()
            Set config.IPAddress = "127.0.0.1"
            Set config.Port = 12345
            Do config.%Save()
        }
        Quit config
    }
}
