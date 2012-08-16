@ECHO OFF
FOR /F %%i IN ('DIR /B /AD %WINDIR%\Microsoft.Net\Framework') DO (
	IF EXIST %WINDIR%\Microsoft.Net\Framework\%%i\csc.exe (
		"%WINDIR%\Microsoft.Net\Framework\%%i\csc.exe" /unsafe /out:"UOExt-Standalone_%%i.exe" /recurse:"Network\*.cs" /recurse:"Plugins\*.cs" /recurse:"Standalone\*.cs" /recurse:"Config.cs" > NUL
		IF EXIST UOExt-Standalone_%%i.exe (
			ECHO Compilation for .Net Framework %%i - success.
		) ELSE (
			ECHO Compilation for .Net Framework %%i - fail.
		)
	)
)
PAUSE