Imports SMath.Manager
Imports SMath.Math
Imports SMath.Math.Numeric
Imports Microsoft.Win32

Public Class Class1
    Implements IPluginHandleEvaluation, IPluginLowLevelEvaluationFast
    Dim asseblyInfos() As AssemblyInfo ' for init
    Private disposedValue As Boolean
    Dim nf As Object
    Dim leng As String = "" : Dim force As String = ""

    Public Sub Initialize() Implements IPlugin.Initialize
        asseblyInfos = New AssemblyInfo() {New AssemblyInfo("SMath Studio", New Version(0, 98), New Guid("a37cba83-b69c-4c71-9992-55ff666763bd"))}
        nf = loadNFapi()
    End Sub

    Public Function GetTermsHandled(sessionProfile As SessionProfile) As TermInfo() Implements IPluginHandleEvaluation.GetTermsHandled
        Dim funcs As New List(Of TermInfo)
        funcs.Add(New TermInfo("nfver", TermType.Function, "Get NextFEM API version", FunctionSections.Unknown, True))
        funcs.Add(New TermInfo("nfopen", TermType.Function, "Open NextFEM model", FunctionSections.Unknown, True, New ArgumentInfo(ArgumentSections.String)))
        funcs.Add(New TermInfo("nfdisp", TermType.Function, "(node,loadcase,type,time) - Get nodal displacement from the selected loadcase and time", FunctionSections.Unknown, True,
                               New ArgumentInfo(ArgumentSections.String), New ArgumentInfo(ArgumentSections.String), New ArgumentInfo(ArgumentSections.RealNumber), New ArgumentInfo(ArgumentSections.RealNumber)))
        funcs.Add(New TermInfo("nfreact", TermType.Function, "(node,loadcase,type,time) - Get nodal reaction from the selected loadcase and time", FunctionSections.Unknown, True,
                               New ArgumentInfo(ArgumentSections.String), New ArgumentInfo(ArgumentSections.String), New ArgumentInfo(ArgumentSections.RealNumber), New ArgumentInfo(ArgumentSections.RealNumber)))
        funcs.Add(New TermInfo("nfbeamforces", TermType.Function, "(beam,loadcase,station,time) - Get beam forces from the selected loadcase and time", FunctionSections.Unknown, True,
                               New ArgumentInfo(ArgumentSections.String), New ArgumentInfo(ArgumentSections.String), New ArgumentInfo(ArgumentSections.RealNumber), New ArgumentInfo(ArgumentSections.RealNumber)))

        Return funcs.ToArray
    End Function

    Protected Overridable Sub Dispose(disposing As Boolean)
        If Not disposedValue Then
            If disposing Then
                nf = Nothing
            End If
            disposedValue = True
        End If
    End Sub

    Protected Overrides Sub Finalize()
        ' Non modificare questo codice. Inserire il codice di pulizia nel metodo 'Dispose(disposing As Boolean)'
        Dispose(disposing:=False)
        MyBase.Finalize()
    End Sub

    Public Sub Dispose() Implements IDisposable.Dispose
        ' Non modificare questo codice. Inserire il codice di pulizia nel metodo 'Dispose(disposing As Boolean)'
        Dispose(disposing:=True)
        GC.SuppressFinalize(Me)
    End Sub

    Public Function TryEvaluateExpression(value As Entry, context As Store, ByRef result As Entry) As Boolean Implements IPluginLowLevelEvaluationFast.TryEvaluateExpression
        ' version - should be a constant
        If value.Type = TermType.Function And value.Text = "nfver" Then
            result = New Entry(CDbl(nf.getVersion()))
            Return True
        End If
        ' open model: https://www.nextfem.it/api/html/M_NextFEMapi_API_openModel.htm
        If value.Type = TermType.Function And value.Text = "nfopen" And value.ArgsCount = 1 Then
            Dim path As String = TermsConverter.DecodeText(Computation.Preprocessing(value.Items(0), context).Text).Replace(Chr(34), "")
            Dim opened As Boolean = nf.openModel(path)
            ' units of measure
            leng = nf.getLenUnit() : force = nf.getForceUnit()
            result = New Entry(If(opened, 1, 0))
            Return True
        End If
        ' nodal displ: https://www.nextfem.it/api/html/M_NextFEMapi_API_getNodalDisp.htm
        If value.Type = TermType.Function And value.ArgsCount >= 4 And value.Text = "nfdisp" Then
            Dim node As String = getStr(value, context, 0)
            Dim lc As String = getStr(value, context, 1)
            Dim dir As String = getStr(value, context, 2) ' Global direction: 1=X, 2=Y, 3=Z, 4=RX, 5=RY, 6=RZ
            Dim time As String = getStr(value, context, 3)  ' time, default 1 (linear analysis)
            Dim val As Double = nf.getNodalDisp(node, lc, time, dir)
            Dim out As New TDouble(val * New TDouble("'" & If(dir < 4, leng, "")))
            result = Entry.Create(out.ToTerms)
            Return True
        End If
        ' nodal react: https://www.nextfem.it/api/html/M_NextFEMapi_API_getNodalReact.htm
        If value.Type = TermType.Function And value.ArgsCount >= 4 And value.Text = "nfreact" Then
            Dim node As String = getStr(value, context, 0)
            Dim lc As String = getStr(value, context, 1)
            Dim type As String = getStr(value, context, 2) ' Global direction: 1=X, 2=Y, 3=Z, 4=RX, 5=RY, 6=RZ
            Dim time As String = getStr(value, context, 3)  ' time, default 1 (linear analysis)
            Dim val As Double = nf.getNodalReact(node, lc, time, type)
            Dim out As New TDouble(val * New TDouble("'" & If(type < 4, force, force & "*'" & leng)))
            result = Entry.Create(out.ToTerms)
            Return True
        End If
        ' beam forces: https://www.nextfem.it/api/html/M_NextFEMapi_API_getBeamForces.htm
        If value.Type = TermType.Function And value.ArgsCount >= 4 And value.Text = "nfbeamforces" Then
            Dim elem As String = getStr(value, context, 0)
            Dim lc As String = getStr(value, context, 1)
            Dim station As String = getStr(value, context, 2) ' Usually a beam has 5 stations (1, 2, 3, 4 or 5)
            Dim time As String = getStr(value, context, 3)  ' time, default 1 (linear analysis)
            Dim val() As Double = nf.getBeamForces(elem, lc, station, time)
            Dim m(5, 0) As TNumber
            For i = 0 To 2
                m(i, 0) = New TNumber(val(i) * New TDouble("'" & force))
            Next
            For i = 3 To 5
                m(i, 0) = New TNumber(val(i) * New TDouble("'" & force & "*'" & leng))
            Next
            Dim mout As New TMatrix(m)
            result = Entry.Create(mout.ToTerms)
            Return True
        End If

        Return False
    End Function

    Function getStr(value As Entry, context As Store, index As Integer) As String
        Try
            getStr = Computation.Preprocessing(value.Items(index), context).Text.Replace(Chr(34), "")
        Catch ex As Exception ' EmptyPlaceholderException
            Return ""
        End Try
    End Function

    Shared Function loadNFapi() As Object
        loadNFapi = Nothing
        ' registry entry for installation path
        Dim iPath As String = Registry.GetValue("HKEY_LOCAL_MACHINE\SOFTWARE\Classes\NextFEM Designer\shell\open\command", "", Nothing)
        If iPath Is Nothing Then
            MsgBox("NextFEM Designer is not installed, exiting")
            Exit Function
        Else
            iPath = IO.Path.GetDirectoryName(iPath.Replace(Chr(34), "").Replace("%1", "").Trim)
        End If
        ' load API
        Dim oAssembly As System.Reflection.Assembly = System.Reflection.Assembly.LoadFrom(iPath & "\NextFEMapi.dll")
        Dim oType As Type = oAssembly.GetType("NextFEMapi.API")
        Return Activator.CreateInstance(oType)
    End Function
End Class