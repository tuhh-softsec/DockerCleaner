# escape = `
ARG REPO=mcr.microsoft.com/dotnet/framework/runtime
FROM $REPO:3.5-windowsservercore-ltsc2016
#  Install .NET 4.8 Fx
RUN powershell -Command $ProgressPreference = 'SilentlyContinue' ; Invoke-WebRequest -UseBasicParsing -Uri "https://download.visualstudio.microsoft.com/download/pr/7afca223-55d2-470a-8edc-6a1739ae3252/abd170b4b0ec15ad0222a809b761a036/ndp48-x86-x64-allos-enu.exe" -OutFile dotnet-framework-installer.exe `
 && .dotnet-framework-installer.exe /q `
 && del .dotnet-framework-installer.exe `
 && powershell Remove-Item -Force -Recurse ${Env:TEMP}*
#  ngen .NET Fx
RUN WindowsMicrosoft.NETFramework64v2.0.50727ngen uninstall "Microsoft.Tpm.Commands, Version=10.0.0.0, Culture=Neutral, PublicKeyToken=31bf3856ad364e35, processorArchitecture=amd64" `
 && WindowsMicrosoft.NETFramework64v4.0.30319ngen update `
 && WindowsMicrosoft.NETFrameworkv4.0.30319ngen update
#  Install NuGet CLI
ENV NUGET_VERSION="4.4.1"
RUN mkdir "%ProgramFiles%\NuGet" `
 && powershell -Command $ProgressPreference = 'SilentlyContinue' ; Invoke-WebRequest -UseBasicParsing -Uri https://dist.nuget.org/win-x86-commandline/v$Env:NUGET_VERSION/nuget.exe -OutFile $Env:ProgramFilesNuGetnuget.exe
#  Install VS components
RUN powershell -Command $ProgressPreference = 'SilentlyContinue' ; Invoke-WebRequest -UseBasicParsing -Uri "https://download.visualstudio.microsoft.com/download/pr/4eef3cca-9da8-417a-889f-53212671e83b/50a7f7a24b21910a7cbe093fa150fd31/vs_testagent.exe" -OutFile vs_TestAgent.exe `
 && start /w vs_TestAgent.exe --quiet --norestart --nocache --wait `
 && del vs_TestAgent.exe `
 && powershell -Command $ProgressPreference = 'SilentlyContinue' ; Invoke-WebRequest -UseBasicParsing -Uri "https://download.visualstudio.microsoft.com/download/pr/4da568ff-b8aa-43ab-92ec-a8129370b49a/6fb89b999fed6f395622e004bfe442eb/vs_buildtools.exe" -OutFile vs_BuildTools.exe `
 && setx /M DOTNET_SKIP_FIRST_TIME_EXPERIENCE 1 `
 && start /w vs_BuildTools.exe ^ --add Microsoft.VisualStudio.Workload.MSBuildTools ^ --add Microsoft.VisualStudio.Workload.NetCoreBuildTools ^ --add Microsoft.Component.ClickOnce.MSBuild ^ --quiet --norestart --nocache --wait `
 && del vs_BuildTools.exe `
 && rmdir /S /Q "%ProgramFiles(x86)%\Microsoft Visual Studio\Installer" `
 && powershell Remove-Item -Force -Recurse "%TEMP%\*" `
 && rmdir /S /Q "%ProgramData%\Package Cache"
#  Install .NET 4.8 SDK
RUN powershell -Command $ErrorActionPreference = 'Stop' ; $ProgressPreference = 'SilentlyContinue' ; Invoke-WebRequest -UseBasicParsing -Uri "https://dotnetbinaries.blob.core.windows.net/dockerassets/sdk_tools48.zip" -OutFile sdk_tools48.zip ; Expand-Archive sdk_tools48.zip `
 && start /w msiexec /i sdk_tools48sdk_tools48.msi /quiet VSEXTUI=1 `
 && rmdir /S /Q sdk_tools48 `
 && del sdk_tools48.zip `
 && powershell Remove-Item -Force -Recurse ${Env:TEMP}*
#  Install web targets
RUN powershell -Command $ErrorActionPreference = 'Stop' ; $ProgressPreference = 'SilentlyContinue' ; Invoke-WebRequest -UseBasicParsing -Uri https://dotnetbinaries.blob.core.windows.net/dockerassets/MSBuild.Microsoft.VisualStudio.Web.targets.2019.05.zip -OutFile MSBuild.Microsoft.VisualStudio.Web.targets.zip ; Expand-Archive MSBuild.Microsoft.VisualStudio.Web.targets.zip -DestinationPath \"${Env:ProgramFiles(x86)}Microsoft Visual Studio2019BuildToolsMSBuildMicrosoftVisualStudiov16.0\" `
 && del MSBuild.Microsoft.VisualStudio.Web.targets.zip
ENV ROSLYN_COMPILER_LOCATION="`"%ProgramFiles(x86)%\Microsoft Visual Studio\2019\BuildTools\MSBuild\Current\Bin\Roslyn`""
#  ngen assemblies queued by VS installers - must be done in cmd shell to avoid access issues
RUN WindowsMicrosoft.NETFramework64v4.0.30319ngen uninstall "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v10.0A\bin\NETFX 4.8 Tools\SecAnnotate.exe" `
 && WindowsMicrosoft.NETFramework64v4.0.30319ngen uninstall "%ProgramFiles(x86)%\Microsoft SDKs\Windows\v10.0A\bin\NETFX 4.8 Tools\WinMDExp.exe" `
 && WindowsMicrosoft.NETFramework64v4.0.30319ngen update `
 && WindowsMicrosoft.NETFrameworkv4.0.30319ngen uninstall "%ProgramFiles(x86)%\Microsoft Visual Studio\2019\TestAgent\Common7\IDE\VSWebLauncher.exe" `
 && WindowsMicrosoft.NETFrameworkv4.0.30319ngen uninstall "%ProgramFiles(x86)%\Microsoft Visual Studio\2019\TestAgent\Common7\IDE\CommonExtensions\Microsoft\Editor\Microsoft.VisualStudio.Diff.CommandLineSwitch.pkgdef" `
 && WindowsMicrosoft.NETFrameworkv4.0.30319ngen uninstall "%ProgramFiles(x86)%\Microsoft Visual Studio\2019\TestAgent\Common7\IDE\CommonExtensions\Microsoft\Editor\Microsoft.VisualStudio.Diff.pkgdef" `
 && WindowsMicrosoft.NETFrameworkv4.0.30319ngen update
#  Set PATH in one layer to keep image size down.
RUN powershell setx /M PATH $( ${Env:PATH} + \" ;${Env:ProgramFiles}NuGet\" + \" ;${Env:ProgramFiles(x86)}Microsoft Visual Studio2019TestAgentCommon7IDECommonExtensionsMicrosoftTestWindow\" + \" ;${Env:ProgramFiles(x86)}Microsoft Visual Studio2019BuildToolsMSBuildCurrentBin\" + \" ;${Env:ProgramFiles(x86)}Microsoft SDKsWindowsv10.0AbinNETFX 4.8 Tools\" + \" ;${Env:ProgramFiles(x86)}Microsoft SDKsClickOnceSignTool\" ;)
#  Install Targeting Packs
RUN powershell " $ErrorActionPreference = 'Stop'; $ProgressPreference = 'SilentlyContinue'; @('4.0', '4.5.2', '4.6.2', '4.7.2', '4.8') | %{ Invoke-WebRequest -UseBasicParsing -Uri https://dotnetbinaries.blob.core.windows.net/referenceassemblies/v${_}.zip -OutFile referenceassemblies.zip; Expand-Archive referenceassemblies.zip -DestinationPath \"${Env:ProgramFiles(x86)}\Reference Assemblies\Microsoft\Framework\.NETFramework\"; Remove-Item -Force referenceassemblies.zip; }"
SHELL ["powershell", "-Command", "$ErrorActionPreference", "=", "'Stop'", ";", "$ProgressPreference", "=", "'SilentlyContinue'"]
