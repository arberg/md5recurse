set CLASS_DIR=d:\Development\BnrDevelopment\private\scala\Md5Tool\bin\production

scala -cp %CLASS_DIR%  Md5Recurse.Md5Recurse --globaldir d:\temp\md5recurseTests -e ISO-8859-1 d:\temp\md5recurseTests\wallpapers --check %*
rem scala d:\Development\BnrDevelopment\private\scala\Md5Tool\out\artifacts\Md5Recurse_jar\Md5Recurse.jar --globaldir d:\temp\md5recurseTests -e ISO-8859-1 d:\temp\md5recurseTests\wallpapers --check %*
 
rem scala -cp %CLASS_DIR% Md5Recurse.Md5Recurse --globaldir d:\temp\md5recurseTests -e ISO-8859-1 d:\temp\md5recurseTests\wallpapers --check %*
rem scala -cp %CLASS_DIR% Md5Recurse.Md5Recurse --globaldir d:\temp\md5recurseTests -e ISO-8859-1 d:\temp\md5recurseTests\test --force -V 2