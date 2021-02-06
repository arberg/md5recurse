# OpenJDK 11 with gradle 5.0: https://stackoverflow.com/questions/46867399/react-native-error-could-not-determine-java-version-from-9-0-1
# $env:JAVA_HOME="C:\Program Files\OpenJDK\jdk-11.0.1"
# "Using java: $env:JAVA_HOME" | Out-Host

# Build with java8, so it works on java8 and newer. Otherwise I need to update java on Tower.
Setup-Environment-Java8-localSession.ps1
.\gradlew build

function install([String]$location) {
    if (Test-Path $location) {
        "Installing to $location" | out-host
        # md5recurse-shadow.zip also works, it has all libs in one jar, but the zip contains dir md5recurse-shadow
        Expand-Archive d:\Development\GitHub\md5recurse\build\distributions\md5recurse.zip $location -force
    } else {
        Write-Host -ForegroundColor Magenta "Target location does not exists: $location"
    }
}
install d:\Development\BnrTools
install d:\Development\BnrSync\Install
install \\towerAlex\home\alex\app
install c:\Tools
install d:\vagrant\win10pro-en-1.2.0\Tools
# Encrypted disk, its prepare task auto-copies it from tower
# install \\nanite\t\bin
