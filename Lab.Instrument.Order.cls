Class Lab.Instrument.Order Extends %Persistent
{
    Property OrderId As %String;
    Property PatientId As %String;
    Property OrderDate As %String;
    Property TestCodes As %String; ; store as $ListBuild(...)
    Property RawData As %String;

    ClassMethod ConvertDelimiters(data As %String)
    {
        Set data = $Replace(data, "^imfd", "|")
        Set data = $Replace(data, "^imcd", "^")
        Quit data
    }

    ClassMethod Validate(order As Lab.Instrument.Order)
    {
        If order.OrderId="" Quit 0
        If order.PatientId="" Quit 0
        If order.OrderDate'?4N1"-"2N1"-"2N Quit 0
        If order.TestCodes="" Quit 0
        Quit 1
    }

    ClassMethod FormatASTM(order As Lab.Instrument.Order)
    {
        Set STX = $Char(2), ETX = $Char(3), CR = $Char(13)
        Set header = "H|\^&|||LIS|||||P|1"
        Set patient = "P|1|"_..ConvertDelimiters(order.PatientId)_"|||||||||||||||||||||||||"
        Set mappedCodes = ""
        Set testCount = $ListLength(order.TestCodes)
        For i=1:1:testCount {
            Set code = $ListGet(order.TestCodes,i)
            Set waspCode = ..InternalToWasp(code,"M")
            Set mappedCodes = $S(mappedCodes'="":mappedCodes_"^",1:"")_waspCode
        }
        Set orderSeg = "O|1|"_..ConvertDelimiters(order.OrderId)_"||"_mappedCodes_"||||||||||||"_..ConvertDelimiters(order.OrderDate)_"||||||||||||||O"
        Set msg = STX_header_CR_patient_CR_orderSeg_CR_ETX
        Quit msg
    }

    ClassMethod PackageOrders(orderList As %String, mode As %String)
    {
        Set result = ""
        Set count = $ListLength(orderList)
        If mode="SINGLE" {
            For i=1:1:count {
                Set orderId = $ListGet(orderList,i)
                Set order = ..%OpenId(orderId)
                Set msg = ..FormatASTM(order)
                Set result = result_msg
            }
        } ElseIf mode="MULTIPLE" {
            Set msg = ""
            For i=1:1:count {
                Set orderId = $ListGet(orderList,i)
                Set order = ..%OpenId(orderId)
                Set msg = msg_..FormatASTM(order)
            }
            Set result = msg
        }
        Quit result
    }

    ClassMethod StorePending(order As Lab.Instrument.Order)
    {
        Do order.%Save()
    }

    ClassMethod TransmitPendingOrders(mode As %String)
    {
        Set orderList = ""
        Set id=""
        For  Set id=$Order(^Lab.Instrument.OrderD(id)) Quit:id=""  {
            Set order = ##class(Lab.Instrument.Order).%OpenId(id)
            If ..Validate(order) {
                Set orderList = orderList_$ListBuild(order.%Id())
            }
        }
        If orderList="" Quit
        Set msg = ..PackageOrders(orderList,mode)
        Set conn = ##class(Lab.Instrument.Connection).%New()
        If conn.IsConnected() {
            Do conn.Socket.Write(msg)
        }
    }

    ClassMethod ReceiveAndParseResults()
    {
        Set conn = ##class(Lab.Instrument.Connection).%New()
        If 'conn.IsConnected() Quit

        Set msg = ""
        Set msg = conn.Socket.Read()
        If msg="" Quit

        Do ..LogParse("Received message: "_msg)

        Set STX = $Char(2), ETX = $Char(3), CR = $Char(13), LF = $Char(10), FS = $Char(28)
        Set msg = $ZStrip(msg, "<W", STX_ETX_CR_LF_FS)

        Set segCount = $Length(msg, CR)
        For segIdx=1:1:segCount {
            Set segment = $Piece(msg, CR, segIdx)
            If segment="" Quit

            Set segType = $Piece(segment, "|", 1)
            If segType="H" {
                Set sender = $Piece(segment, "|", 5)
                If sender'="WASPLAB" {
                    Do ..LogParse("Ignored message from sender: "_sender)
                    Quit
                }
            }
            ElseIf segType="O" {
                Set orderId = $Piece(segment, "|", 3)
                Set orderDate = ..ParseASTMDate($Piece(segment, "|", 15))
                Set patientId = ""
                Set currentOrder = orderId
                Set ^ParsedResults(currentOrder,"OrderDate") = orderDate
            }
            ElseIf segType="R" {
                Set waspCode = $Piece($Piece(segment, "|", 3), "^", 1)
                Set testCode = ..WaspToInternal(waspCode,"M")
                Set resultValue = $Piece(segment, "|", 4)
                Set resultDate = ..ParseASTMDate($Piece(segment, "|", 13))
                Set resultTime = ..ParseASTMTime($Piece(segment, "|", 14))
                Set ^ParsedResults(currentOrder,"Results",testCode,"Value") = resultValue
                Set ^ParsedResults(currentOrder,"Results",testCode,"Date") = resultDate
                Set ^ParsedResults(currentOrder,"Results",testCode,"Time") = resultTime
            }
            ElseIf segType="L" {
                Do ..LogParse("Trailer segment processed.")
            }
        }

        Set order=""
        For  Set order=$Order(^ParsedResults(order)) Quit:order=""  {
            Set test=""
            For  Set test=$Order(^ParsedResults(order,"Results",test)) Quit:test=""  {
                Set value = ^ParsedResults(order,"Results",test,"Value")
                Set date = ^ParsedResults(order,"Results",test,"Date")
                Set time = ^ParsedResults(order,"Results",test,"Time")
                Set ^LISResults(order,test,"Value") = value
                Set ^LISResults(order,test,"Date") = date
                Set ^LISResults(order,test,"Time") = time
            }
        }
        Do ..LogParse("Parsed and transformed results for orders.")
    }

    ClassMethod ParseASTMDate(astmDate As %String)
    {
        If astmDate?8N {
            Quit $Extract(astmDate,1,4)_"-"_$Extract(astmDate,5,2)_"-"_$Extract(astmDate,7,2)
        }
        Quit ""
    }

    ClassMethod ParseASTMTime(astmTime As %String)
    {
        If astmTime?6N {
            Quit $Extract(astmTime,1,2)_":"_$Extract(astmTime,3,2)_":"_$Extract(astmTime,5,2)
        }
        Quit ""
    }

    ClassMethod LogParse(msg As %String)
    {
        Set ^LabInstrumentParseLog($Increment(^LabInstrumentParseLog)) = $ZDATETIME($H,3)_" "_msg
    }

    ClassMethod GetOrderDBConfig()
    {
        Set config("RetentionDays") = $Get(^OrderDBConfig("RetentionDays"),7)
        Set config("SpecimenPadChar") = $Get(^OrderDBConfig("SpecimenPadChar"),"0")
        Set config("SpecimenSigDigits") = $Get(^OrderDBConfig("SpecimenSigDigits"),8)
        Set config("SpecimenStripLeadingZeroes") = $Get(^OrderDBConfig("SpecimenStripLeadingZeroes"),1)
        Set config("OnDuplicateAction") = $Get(^OrderDBConfig("OnDuplicateAction"),"Overwrite")
        Quit $Name(config)
    }

    ClassMethod FormatSpecimenID(specimenId As %String)
    {
        Set configRef = ..GetOrderDBConfig()
        Set id = specimenId
        If @configRef@("SpecimenStripLeadingZeroes") {
            Set id = +id
        }
        Set id = $Justify(id,@configRef@("SpecimenSigDigits"),@configRef@("SpecimenPadChar"))
        Quit id
    }

    ClassMethod StoreOrUpdatePendingOrder(order As Lab.Instrument.Order)
    {
        Set configRef = ..GetOrderDBConfig()
        Set specimenId = ..FormatSpecimenID(order.OrderId)
        Set action = @configRef@("OnDuplicateAction")
        If $Data(^PendingOrders(specimenId)) {
            If action="Overwrite" {
                Kill ^PendingOrders(specimenId)
                Set ^PendingOrders(specimenId) = $ListBuild(order.%Id())
            } ElseIf action="Append" {
                Set list = $Get(^PendingOrders(specimenId))
                Set list = list_$ListBuild(order.%Id())
                Set ^PendingOrders(specimenId) = list
            }
        } Else {
            Set ^PendingOrders(specimenId) = $ListBuild(order.%Id())
        }
    }

    ClassMethod RemoveOnQuery(specimenId As %String)
    {
        Set specimenId = ..FormatSpecimenID(specimenId)
        Kill ^PendingOrders(specimenId)
    }

    ClassMethod RemoveOnResult(specimenId As %String)
    {
        Set specimenId = ..FormatSpecimenID(specimenId)
        Kill ^PendingOrders(specimenId)
    }

    ClassMethod RemoveOnCancel(specimenId As %String)
    {
        Set specimenId = ..FormatSpecimenID(specimenId)
        Kill ^PendingOrders(specimenId)
    }

    ClassMethod PurgeOldOrders()
    {
        Set configRef = ..GetOrderDBConfig()
        Set cutoff = $H - @configRef@("RetentionDays")
        Set id=""
        For  Set id=$Order(^PendingOrders(id)) Quit:id=""  {
            Set orderList = ^PendingOrders(id)
            Set keep=0
            For i=1:1:$ListLength(orderList) {
                Set orderId = $ListGet(orderList,i)
                Set order = ..%OpenId(orderId)
                If $ZDateH(order.%TimeStamp,8) >= cutoff Set keep=1
            }
            If 'keep Kill ^PendingOrders(id)
        }
    }

    ClassMethod SetDuplicateAction(action As %String)
    {
        Set ^OrderDBConfig("OnDuplicateAction") = action
    }

    ClassMethod SetSpecimenFormat(stripZeroes, sigDigits, padChar)
    {
        Set ^OrderDBConfig("SpecimenStripLeadingZeroes") = stripZeroes
        Set ^OrderDBConfig("SpecimenSigDigits") = sigDigits
        Set ^OrderDBConfig("SpecimenPadChar") = padChar
    }

    ClassMethod ShowOrderDBConfig()
    {
        Write "RetentionDays: ", $Get(^OrderDBConfig("RetentionDays"),7), !
        Write "OnDuplicateAction: ", $Get(^OrderDBConfig("OnDuplicateAction"),"Overwrite"), !
        Write "SpecimenStripLeadingZeroes: ", $Get(^OrderDBConfig("SpecimenStripLeadingZeroes"),1), !
        Write "SpecimenSigDigits: ", $Get(^OrderDBConfig("SpecimenSigDigits"),8), !
        Write "SpecimenPadChar: ", $Get(^OrderDBConfig("SpecimenPadChar"),"0"), !
    }

    /// Persistent mapping global: ^TestCodeMap("internal",internalCode,type)=waspCode
    ///                         : ^TestCodeMap("wasp",waspCode,type)=internalCode

    /// Add or update a test code mapping (with validation and logging)
    ClassMethod AddTestCodeMapping(internalCode As %String, waspCode As %String, type As %String, user As %String)
    {
        // type: "M", "G", or "W"
        If (internalCode="")!(waspCode="") Quit $$$ERROR
        If $Data(^TestCodeMap("internal",internalCode,type)) || $Data(^TestCodeMap("wasp",waspCode,type)) {
            Quit $$$ERROR // Prevent duplicate mapping
        }
        Set ^TestCodeMap("internal",internalCode,type) = waspCode
        Set ^TestCodeMap("wasp",waspCode,type) = internalCode
        Do ..LogMapping("ADD",internalCode,waspCode,type,user)
        Quit $$$OK
    }

    /// Remove a test code mapping (with logging)
    ClassMethod RemoveTestCodeMapping(internalCode As %String, type As %String, user As %String) As %Status
    {
        Set waspCode = $Get(^TestCodeMap("internal",internalCode,type))
        If waspCode'="" {
            Kill ^TestCodeMap("internal",internalCode,type)
            Kill ^TestCodeMap("wasp",waspCode,type)
            Do ..LogMapping("REMOVE",internalCode,waspCode,type,user)
            Quit $$$OK
        }
        Quit $$$ERROR
    }

    /// Translate internal code to WASPLab code (for order transmission)
    ClassMethod InternalToWasp(internalCode As %String, type As %String) As %String
    {
        Quit $Get(^TestCodeMap("internal",internalCode,type), internalCode)
    }

    /// Translate WASPLab code to internal code (for result parsing)
    ClassMethod WaspToInternal(waspCode As %String, type As %String) As %String
    {
        Quit $Get(^TestCodeMap("wasp",waspCode,type), waspCode)
    }

    /// Search/filter mappings (by partial code or type)
    ClassMethod SearchMappings(query As %String = "", type As %String = "") As %ListOfDataTypes
    {
        Set result = ##class(%ListOfDataTypes).%New()
        Set code=""
        For  Set code=$Order(^TestCodeMap("internal",code,type)) Quit:code=""  {
            Set waspCode = ^TestCodeMap("internal",code,type)
            If (query="")!($Find(code,query))!($Find(waspCode,query)) {
                Do result.Insert(code_"|"_waspCode_"|"_type)
            }
        }
        Quit result
    }

    /// Log mapping changes with timestamp and user
    ClassMethod LogMapping(action As %String, internalCode As %String, waspCode As %String, type As %String, user As %String)
    {
        Set ^TestCodeMapLog($Increment(^TestCodeMapLog)) = $ZDATETIME($H,3)_"|"_action_"|"_internalCode_"|"_waspCode_"|"_type_"|"_user
    }

    /// Show mapping log
    ClassMethod ShowMappingLog()
    {
        Set i=""
        For  Set i=$Order(^TestCodeMapLog(i)) Quit:i=""  {
            Write ^TestCodeMapLog(i),!
        }
    }

    /// Show all mappings (optionally filtered)
    ClassMethod ShowMappings(query As %String = "", type As %String = "")
    {
        Set mappings = ..SearchMappings(query, type)
        Write "InternalCode | WASPLabCode | Type",!
        For i=1:1:mappings.Count() {
            Write mappings.GetAt(i),!
        }
    }

    /// Override: FormatASTM to use mapped WASPLab codes
    ClassMethod FormatASTM(order As Lab.Instrument.Order)
    {
        Set STX = $Char(2), ETX = $Char(3), CR = $Char(13)
        Set header = "H|\^&|||LIS|||||P|1"
        Set patient = "P|1|"_..ConvertDelimiters(order.PatientId)_"|||||||||||||||||||||||||"
        // Map all test codes to WASPLab codes (M-type by default)
        Set mappedCodes = ""
        For i=1:1:$ListLength(order.TestCodes) {
            Set code = $ListGet(order.TestCodes,i)
            Set waspCode = ..InternalToWasp(code,"M")
            Set mappedCodes = $S(mappedCodes'="":mappedCodes_"^",1:"")_waspCode
        }
        Set orderSeg = "O|1|"_..ConvertDelimiters(order.OrderId)_"||"_mappedCodes_"||||||||||||"_..ConvertDelimiters(order.OrderDate)_"||||||||||||||O"
        Set msg = STX_header_CR_patient_CR_orderSeg_CR_ETX
        Quit msg
    }

    /// Override: ReceiveAndParseResults to map WASPLab codes to internal codes
    ClassMethod ReceiveAndParseResults()
    {
        // Assume connection is already established and socket is available
        Set conn = ##class(Lab.Instrument.Connection).%New()
        If 'conn.IsConnected() Quit

        // Read incoming ASTM message (blocking read for demonstration)
        Set msg = ""
        Try {
            Set msg = conn.Socket.Read()
        } Catch ex {
            Do ..LogParse("Error reading from socket: "_ex.DisplayString())
            Quit
        }
        If msg="" Quit

        // Log the raw message
        Do ..LogParse("Received message: "_msg)

        // Handle ASTM control characters
        Set STX = $Char(2), ETX = $Char(3), CR = $Char(13), LF = $Char(10), FS = $Char(28)
        Set msg = $ZStrip(msg, "<W", STX_ETX_CR_LF_FS)

        // Split into segments by CR
        Set segCount = $Length(msg, CR)
        For segIdx=1:1:segCount {
            Set segment = $Piece(msg, CR, segIdx)
            If segment="" Continue

            Set segType = $Piece(segment, "|", 1)
            If segType="H" {
                // Header: check sender
                Set sender = $Piece(segment, "|", 5)
                If sender'="WASPLAB" {
                    Do ..LogParse("Ignored message from sender: "_sender)
                    Quit
                }
            }
            ElseIf segType="O" {
                // Order segment: parse order ID, etc.
                Set orderId = $Piece(segment, "|", 3)
                Set orderDate = ..ParseASTMDate($Piece(segment, "|", 15))
                Set patientId = "" // May be in P segment
                Set currentOrder = orderId
                Set ^ParsedResults(currentOrder,"OrderDate") = orderDate
            }
            ElseIf segType="R" {
                // Result segment: parse result fields
                Set waspCode = $Piece($Piece(segment, "|", 3), "^", 1)
                Set testCode = ..WaspToInternal(waspCode,"M")
                Set resultValue = $Piece(segment, "|", 4)
                Set resultDate = ..ParseASTMDate($Piece(segment, "|", 13))
                Set resultTime = ..ParseASTMTime($Piece(segment, "|", 14))
                Set ^ParsedResults(currentOrder,"Results",testCode,"Value") = resultValue
                Set ^ParsedResults(currentOrder,"Results",testCode,"Date") = resultDate
                Set ^ParsedResults(currentOrder,"Results",testCode,"Time") = resultTime
            }
            ElseIf segType="L" {
                // Trailer segment: end of message
                Do ..LogParse("Trailer segment processed.")
            }
            // Optionally handle Patient (P) and other segments as needed
        }

        // Transform parsed data to internal format (example: move to LIS structure)
        Set order=""
        For  Set order=$Order(^ParsedResults(order)) Quit:order=""  {
            // Example transformation: store in ^LISResults global
            Set test=""
            For  Set test=$Order(^ParsedResults(order,"Results",test)) Quit:test=""  {
                Set value = ^ParsedResults(order,"Results",test,"Value")
                Set date = ^ParsedResults(order,"Results",test,"Date")
                Set time = ^ParsedResults(order,"Results",test,"Time")
                Set ^LISResults(order,test,"Value") = value
                Set ^LISResults(order,test,"Date") = date
                Set ^LISResults(order,test,"Time") = time
            }
        }
        Do ..LogParse("Parsed and transformed results for orders.")
    }

    /// Helper: Parse ASTM date (YYYYMMDD) to internal format (YYYY-MM-DD)
    ClassMethod ParseASTMDate(astmDate As %String) As %String
    {
        If astmDate?8N {
            Quit $Extract(astmDate,1,4)_"-"_$Extract(astmDate,5,2)_"-"_$Extract(astmDate,7,2)
        }
        Quit ""
    }

    /// Helper: Parse ASTM time (HHMMSS) to internal format (HH:MM:SS)
    ClassMethod ParseASTMTime(astmTime As %String) As %String
    {
        If astmTime?6N {
            Quit $Extract(astmTime,1,2)_":"_$Extract(astmTime,3,2)_":"_$Extract(astmTime,5,2)
        }
        Quit ""
    }

    /// Log parsing activities for troubleshooting and audit
    ClassMethod LogParse(msg As %String)
    {
        Set ^LabInstrumentParseLog($Increment(^LabInstrumentParseLog)) = $ZDATETIME($H,3)_" "_msg
    }

    /// Pending order management configuration
    ClassMethod GetOrderDBConfig() As %DynamicObject
    {
        // Example config: can be loaded from persistent config class or global
        Set config = ##class(%DynamicObject).%New()
        Set config.RetentionDays = 7
        Set config.SpecimenPadChar = "0"
        Set config.SpecimenSigDigits = 8
        Set config.SpecimenStripLeadingZeroes = 1
        Set config.OnDuplicateAction = "Overwrite" // or "Append"
        Quit config
    }

    /// Format Specimen ID according to configuration
    ClassMethod FormatSpecimenID(specimenId As %String) As %String
    {
        Set config = ..GetOrderDBConfig()
        Set id = specimenId
        If config.SpecimenStripLeadingZeroes {
            Set id = +id
        }
        Set id = $Justify(id,config.SpecimenSigDigits,config.SpecimenPadChar)
        Quit id
    }

    /// Store or update a pending order (handles Overwrite/Append)
    ClassMethod StoreOrUpdatePendingOrder(order As Lab.Instrument.Order)
    {
        Set config = ..GetOrderDBConfig()
        Set specimenId = ..FormatSpecimenID(order.OrderId)
        Set action = config.OnDuplicateAction
        If $Data(^PendingOrders(specimenId)) {
            If action="Overwrite" {
                Kill ^PendingOrders(specimenId)
                Set ^PendingOrders(specimenId) = $ListBuild(order.%Id())
            } ElseIf action="Append" {
                Set list = $Get(^PendingOrders(specimenId))
                Set list = list_$ListBuild(order.%Id())
                Set ^PendingOrders(specimenId) = list
            }
        } Else {
            Set ^PendingOrders(specimenId) = $ListBuild(order.%Id())
        }
    }

    /// Remove tests/orders when analyzer queries for orders
    ClassMethod RemoveOnQuery(specimenId As %String)
    {
        Set specimenId = ..FormatSpecimenID(specimenId)
        Kill ^PendingOrders(specimenId)
    }

    /// Remove tests/orders when analyzer transmits results
    ClassMethod RemoveOnResult(specimenId As %String)
    {
        Set specimenId = ..FormatSpecimenID(specimenId)
        Kill ^PendingOrders(specimenId)
    }

    /// Remove tests/orders when cancel messages are processed
    ClassMethod RemoveOnCancel(specimenId As %String)
    {
        Set specimenId = ..FormatSpecimenID(specimenId)
        Kill ^PendingOrders(specimenId)
    }

    /// Purge orders older than retention policy
    ClassMethod PurgeOldOrders()
    {
        Set config = ..GetOrderDBConfig()
        Set cutoff = $H - config.RetentionDays
        Set id=""
        For  Set id=$Order(^PendingOrders(id)) Quit:id=""  {
            Set orderList = ^PendingOrders(id)
            Set keep=0
            For i=1:1:$ListLength(orderList) {
                Set orderId = $ListGet(orderList,i)
                Set order = ..%OpenId(orderId)
                If $ZDateH(order.%TimeStamp,8) >= cutoff Set keep=1
            }
            If 'keep Kill ^PendingOrders(id)
        }
    }

    /// UI method: configure default action for duplicate specimen IDs
    ClassMethod SetDuplicateAction(action As %String)
    {
        // action: "Overwrite" or "Append"
        Set ^OrderDBConfig("OnDuplicateAction") = action
    }

    /// UI method: configure specimen formatting
    ClassMethod SetSpecimenFormat(stripZeroes As %Boolean, sigDigits As %Integer, padChar As %String)
    {
        Set ^OrderDBConfig("SpecimenStripLeadingZeroes") = stripZeroes
        Set ^OrderDBConfig("SpecimenSigDigits") = sigDigits
        Set ^OrderDBConfig("SpecimenPadChar") = padChar
    }

    /// UI method: get current config
    ClassMethod ShowOrderDBConfig()
    {
        Write "RetentionDays: ", $Get(^OrderDBConfig("RetentionDays"),7), !
        Write "OnDuplicateAction: ", $Get(^OrderDBConfig("OnDuplicateAction"),"Overwrite"), !
        Write "SpecimenStripLeadingZeroes: ", $Get(^OrderDBConfig("SpecimenStripLeadingZeroes"),1), !
        Write "SpecimenSigDigits: ", $Get(^OrderDBConfig("SpecimenSigDigits"),8), !
        Write "SpecimenPadChar: ", $Get(^OrderDBConfig("SpecimenPadChar"),"0"), !
    }

    /// Persistent mapping global: ^InstrumentIDMap("internal",internalID)=waspID
    ///                         : ^InstrumentIDMap("wasp",waspID)=internalID

    /// Add or update an instrument ID mapping (with validation and logging)
    ClassMethod AddInstrumentIDMapping(internalID As %String, waspID As %String, user As %String) As %Status
    {
        If (internalID="")!(waspID="") Quit $$$ERROR
        Set ^InstrumentIDMap("internal",internalID) = waspID
        Set ^InstrumentIDMap("wasp",waspID) = internalID
        Do ..LogInstrumentIDMapping("ADD",internalID,waspID,user)
        Quit $$$OK
    }

    /// Remove an instrument ID mapping (with logging)
    ClassMethod RemoveInstrumentIDMapping(internalID As %String, user As %String) As %Status
    {
        Set waspID = $Get(^InstrumentIDMap("internal",internalID))
        If waspID'="" {
            Kill ^InstrumentIDMap("internal",internalID)
            Kill ^InstrumentIDMap("wasp",waspID)
            Do ..LogInstrumentIDMapping("REMOVE",internalID,waspID,user)
            Quit $$$OK
        }
        Quit $$$ERROR
    }

    /// Translate internal instrument ID to WASPLab instrument ID
    ClassMethod InternalToWaspInstrumentID(internalID As %String) As %String
    {
        Quit $Get(^InstrumentIDMap("internal",internalID), internalID)
    }

    /// Translate WASPLab instrument ID to internal instrument ID
    ClassMethod WaspToInternalInstrumentID(waspID As %String) As %String
    {
        Quit $Get(^InstrumentIDMap("wasp",waspID), waspID)
    }

    /// Show all instrument ID mappings
    ClassMethod ShowInstrumentIDMappings()
    {
        Write "InternalID | WASPLabID",!
        Set internalID=""
        For  Set internalID=$Order(^InstrumentIDMap("internal",internalID)) Quit:internalID=""  {
            Write internalID," | ",^InstrumentIDMap("internal",internalID),!
        }
    }

    /// Log instrument ID mapping changes
    ClassMethod LogInstrumentIDMapping(action As %String, internalID As %String, waspID As %String, user As %String)
    {
        Set ^InstrumentIDMapLog($Increment(^InstrumentIDMapLog)) = $ZDATETIME($H,3)_"|"_action_"|"_internalID_"|"_waspID_"|"_user
    }

    /// Show instrument ID mapping log
    ClassMethod ShowInstrumentIDMappingLog()
    {
        Set i=""
        For  Set i=$Order(^InstrumentIDMapLog(i)) Quit:i=""  {
            Write ^InstrumentIDMapLog(i),!
        }
    }

    /// Log instrument identification errors
    ClassMethod LogInstrumentIDError(msg As %String)
    {
        Set ^InstrumentIDErrorLog($Increment(^InstrumentIDErrorLog)) = $ZDATETIME($H,3)_" "_msg
    }

    /// Show instrument identification errors
    ClassMethod ShowInstrumentIDErrors()
    {
        Set i=""
        For  Set i=$Order(^InstrumentIDErrorLog(i)) Quit:i=""  {
            Write ^InstrumentIDErrorLog(i),!
        }
    }

    /// Example: Use instrument ID mapping in ASTM message parsing (Header segment)
    ClassMethod ParseHeaderSegment(segment As %String) As %String
    {
        // segment: ASTM header segment string
        // Extract sender (WASPLAB instrument ID) from field 5
        Set waspID = $Piece(segment, "|", 5)
        If waspID="" {
            Do ..LogInstrumentIDError("Empty instrument ID in header segment: "_segment)
            Quit ""
        }
        Set internalID = ..WaspToInternalInstrumentID(waspID)
        If internalID="" {
            Do ..LogInstrumentIDError("Unknown WASPLab instrument ID: "_waspID)
        }
        Quit internalID
    }

    /// Example: Use instrument ID mapping in ASTM message construction (Header segment)
    ClassMethod BuildHeaderSegment(internalID As %String) As %String
    {
        Set waspID = ..InternalToWaspInstrumentID(internalID)
        Quit "H|\^&|||" _ waspID _ "|||||P|1"
    }

    /// UI: Display all communication data, errors, and trace for WASPLab driver
    ClassMethod ShowCommunicationUI()
    {
        // Driver info
        Set driverName = "Copan WASPLab Instrument Driver"
        Set driverVersion = "v1.0.0"
        Write "==== ", driverName, " (", driverVersion, ") ====", !

        // Outgoing messages
        Write !,"--- Outgoing Messages ---",!
        Set i=""
        For  Set i=$Order(^LabInstrumentLog(i)) Quit:i=""  {
            Write "[OUT] ", ^LabInstrumentLog(i), !
        }

        // Incoming messages
        Write !,"--- Incoming Messages ---",!
        Set i=""
        For  Set i=$Order(^LabInstrumentParseLog(i)) Quit:i=""  {
            Write "[IN] ", ^LabInstrumentParseLog(i), !
        }

        // Communication trace (driver, version, timestamps)
        Write !,"--- Communication Trace ---",!
        Set i=""
        For  Set i=$Order(^LabInstrumentTrace(i)) Quit:i=""  {
            Write "[TRACE] ", ^LabInstrumentTrace(i), !
        }

        // Error messages (prominent display)
        Write !,"--- Communication Errors ---",!
        Set i=""
        For  Set i=$Order(^InstrumentIDErrorLog(i)) Quit:i=""  {
            Write "**ERROR** ", ^InstrumentIDErrorLog(i), !
        }
        Set i=""
        For  Set i=$Order(^LabInstrumentErrorLog(i)) Quit:i=""  {
            Write "**ERROR** ", ^LabInstrumentErrorLog(i), !
        }

        Write !,"==== End of Communication Data ====",!
    }

    /// UI: Log outgoing message (for trace)
    ClassMethod LogOutgoing(msg As %String)
    {
        Set ^LabInstrumentLog($Increment(^LabInstrumentLog)) = $ZDATETIME($H,3)_" "_msg
        Set ^LabInstrumentTrace($Increment(^LabInstrumentTrace)) = "OUT|" _ $ZDATETIME($H,3) _ "|" _ msg
    }

    /// UI: Log incoming message (for trace)
    ClassMethod LogIncoming(msg As %String)
    {
        Set ^LabInstrumentParseLog($Increment(^LabInstrumentParseLog)) = $ZDATETIME($H,3)_" "_msg
        Set ^LabInstrumentTrace($Increment(^LabInstrumentTrace)) = "IN|" _ $ZDATETIME($H,3) _ "|" _ msg
    }

    /// UI: Log communication error (for trace)
    ClassMethod LogCommError(msg As %String)
    {
        Set ^LabInstrumentErrorLog($Increment(^LabInstrumentErrorLog)) = $ZDATETIME($H,3)_" "_msg
        Set ^LabInstrumentTrace($Increment(^LabInstrumentTrace)) = "ERROR|" _ $ZDATETIME($H,3) _ "|" _ msg
    }

}
