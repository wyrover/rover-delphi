Function ReplaceFileContent(filepath, pattern, text, is_utf8)
	Set objFSO = CreateObject("Scripting.FileSystemObject")
	Set objFile = objFSO.GetFile(filepath)
	Dim objStream

	' Valid Charset values for ADODB.Stream
    Const CdoBIG5        = "big5"
    Const CdoEUC_JP      = "euc-jp"
    Const CdoEUC_KR      = "euc-kr"
    Const CdoGB2312      = "gb2312"
    Const CdoISO_2022_JP = "iso-2022-jp"
    Const CdoISO_2022_KR = "iso-2022-kr"
    Const CdoISO_8859_1  = "iso-8859-1"
    Const CdoISO_8859_2  = "iso-8859-2"
    Const CdoISO_8859_3  = "iso-8859-3"
    Const CdoISO_8859_4  = "iso-8859-4"
    Const CdoISO_8859_5  = "iso-8859-5"
    Const CdoISO_8859_6  = "iso-8859-6"
    Const CdoISO_8859_7  = "iso-8859-7"
    Const CdoISO_8859_8  = "iso-8859-8"
    Const CdoISO_8859_9  = "iso-8859-9"
    Const cdoKOI8_R      = "koi8-r"
    Const cdoShift_JIS   = "shift-jis"
    Const CdoUS_ASCII    = "us-ascii"
    Const CdoUTF_7       = "utf-7"
    Const CdoUTF_8       = "utf-8"

    ' ADODB.Stream file I/O constants
    Const adTypeBinary          = 1
    Const adTypeText            = 2
    Const adSaveCreateNotExist  = 1
    Const adSaveCreateOverWrite = 2


	If objFile.Size > 0 Then
		
		If is_utf8 = 1 Then			
			Set objStream = CreateObject( "ADODB.Stream" )
			objStream.Open
			objStream.Type = adTypeText
			objStream.Position = 0
			objStream.Charset = CdoUTF_8
			objStream.LoadFromFile filepath
			strContents = objstream.ReadText			
			objStream.Close
			Set objStream = Nothing
		Else
			Set objReadFile = objFSO.OpenTextFile(filepath, 1)
			strContents = objReadFile.ReadAll
			objReadFile.Close		
		End If
	End If

	Dim re
	Set re = new RegExp
	re.IgnoreCase = False
	re.Global = True
	re.MultiLine = True
	re.Pattern = pattern
	strContents = re.replace(strContents, text)

	're.Pattern="^Public\s+Const\s+APP_VERSION.*""$"
	'strContents = re.replace(strContents,"Public Const APP_VERSION = ""Version: " & appversion & """")

	Set re = Nothing	
	
	If is_utf8 = 1 Then
		Set objStream = CreateObject( "ADODB.Stream" )
		objStream.Open
		objStream.Type = adTypeText
		objStream.Position = 0
		objStream.Charset = CdoUTF_8
		objStream.WriteText = strContents
		objStream.SaveToFile filepath, adSaveCreateOverWrite
		objStream.Close
		Set objStream = Nothing
	Else
		Set objWriteFile = objFSO.OpenTextFile(filepath, 2, False)
		objWriteFile.Write(strContents)		
		objWriteFile.Close	
	End If
End Function

Function ReFilename(filepath, name)
	Set objFSO = CreateObject("Scripting.FileSystemObject")
	Set objFile = objFSO.GetFile(filepath)	
	objFile.Name = name
	Set objFSO = Nothing
End Function

Function ReDir(source, dest)
	Set objFSO = CreateObject("Scripting.FileSystemObject")
	objFSO.MoveFolder source, dest
	Set objFSO = Nothing
End Function

Function DisposePath(sPath)
    On Error Resume Next

    If Right(sPath, 1) = "\" Then
        DisposePath = sPath
    Else
        DisposePath = sPath & "\"
    End If

    DisposePath = Trim(DisposePath)
End Function

Function NewGUID
	Set TypeLib = CreateObject("Scriptlet.TypeLib") 
    NewGUID = Left(TypeLib.Guid, 38)
	Set TypeLib = Nothing
End Function


Function NewGUID2  
  Set TypeLib = CreateObject("Scriptlet.TypeLib")
  NewGUID2 = Mid(TypeLib.Guid, 2, 36)
  Set TypeLib = Nothing
End Function 

Function FolderExists(DirPath)
	Dim fso
	FolderExists = False
	Set fso = CreateObject("Scripting.FileSystemObject")
	If fso.FolderExists(DirPath) <> 0 Then
		FolderExists = True
	End If
End Function