Function Include(file)
	ExecuteGlobal CreateObject("Scripting.FileSystemObject").OpenTextFile(file,1).ReadAll
End Function

'//---------------------------------------------------------------
'// 程序入口
'//---------------------------------------------------------------

Include "common.vbs"

Dim WinTitle
Dim PROJECT_DIR
Dim PROJECT_NAME

WinTitle = "创建delphi测试工程"



Sub Inputlocation()
	Dim objShell
	Set objShell=wscript.createObject("wscript.shell")
	objShell.CurrentDirectory = "..\src"
	PROJECT_DIR = DisposePath(objShell.CurrentDirectory)
End Sub 

Sub InputProjectName()
	PROJECT_NAME = InputBox("项目名称", WinTitle)

	If PROJECT_NAME = False Then
		Wscript.Quit		
	End If 
	
	PROJECT_DIR = DisposePath(PROJECT_DIR & PROJECT_NAME)
End Sub 

Sub CreateProject()
	On Error Resume Next
	Dim objShell
	Set objShell = wscript.createObject("wscript.shell")
	objShell.CurrentDirectory = left(Wscript.ScriptFullName,len(Wscript.ScriptFullName)-len(Wscript.ScriptName))
	iReturn = objShell.Run("7z x ""test_01.7z"" -y -o""" & PROJECT_DIR  & """", 1, TRUE)


	Call ReplaceFileContent(PROJECT_DIR & "test_01.dpr", "test_01", PROJECT_NAME, 0) 
	Call ReplaceFileContent(PROJECT_DIR & "test_01.dproj", "test_01", PROJECT_NAME, 1) 
	Call ReFilename(PROJECT_DIR & "test_01.dpr", PROJECT_NAME & ".dpr")
	Call ReFilename(PROJECT_DIR & "test_01.dproj", PROJECT_NAME & ".dproj")
	Call ReFilename(PROJECT_DIR & "test_01.dof", PROJECT_NAME & ".dof")
End Sub
 



Call Inputlocation
Call InputProjectName
Call CreateProject

Wscript.Quit
