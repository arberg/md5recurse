# OpenJDK 11 with gradle 5.0: https://stackoverflow.com/questions/46867399/react-native-error-could-not-determine-java-version-from-9-0-1
# $env:JAVA_HOME="C:\Program Files\OpenJDK\jdk-11.0.1"
# "Using java: $env:JAVA_HOME" | Out-Host

.\gradlew build

function install([String]$location) {
    "Installing to $location" | out-host
    Expand-Archive d:\Development\GitHub\md5recurse\build\distributions\md5recurse.zip  -DestinationPath $location -force
}
install d:\Development\BnrTools
install \\towerAlex\flash\app
install c:\Tools
install d:\vagrant\win10pro-en-1.2.0\Tools
