
here are the steps to prepare keystore for both client and server,
given cert.crt and cert.key files

NOTE:
You can't directly import private key information to a keystore (.JKS) using keytool.
Instead, you must convert the certificate and private key into a PKCS 12 (.p12) file,
and then you can import the PKCS 12 file into your keystore.

** first create .p12 keystore **

openssl pkcs12 -export -in cert.crt -inkey cert.key -name localhost -out cert-PKCS-12.p12

** then import .p12 keystore into a new JKS keystore **

keytool -importkeystore -deststorepass 123456 -destkeystore rpcserver2.jks -srckeystore cert-PKCS-12.p12 -srcstoretype PKCS12


** then import our certificate to cacerts (list of certificate authorities) **

cd /Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/jre/lib/security
keytool -import -alias localhost -file /Users/miloszm/proj/epsmi/cert.crt -keystore cacerts

(it will ask for password - the password is "changeit")

this way our certificate will be treated as trusted

note that cacert solution can be replaced with setting the -Djavax.net.ssl.trustStore=/Users/miloszm/proj/epsmi/rpcserver2.jks
(the same as keystore, this seems to be working)
so all three settings are as follows:
-Djavax.net.ssl.keyStore=/Users/miloszm/proj/epsmi/rpcserver2.jks
-Djavax.net.ssl.keyStorePassword=123456
-Djavax.net.ssl.trustStore=/Users/miloszm/proj/epsmi/rpcserver2.jks


** then set the JVM settings for our keystore to be used **

-Djavax.net.ssl.keyStore=/Users/miloszm/proj/epsmi/rpcserver2.jks -Djavax.net.ssl.keyStorePassword=123456


** was is useful when troubleshooting ssl: **
-Djavax.net.debug=ssl:all

Note that here we are setting keystore using keytool rather than doing it programmatically.
Also, we are changing cacerts located at: /Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/jre/lib/security

