java -Djavax.net.ssl.keyStore=/Users/miloszm/proj/epsmi/rpcserver2.jks -Djavax.net.ssl.keyStorePassword=123456 -cp /Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/jre/lib/charsets.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/jre/lib/ext/cldrdata.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/jre/lib/ext/dnsns.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/jre/lib/ext/jaccess.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/jre/lib/ext/localedata.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/jre/lib/ext/nashorn.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/jre/lib/ext/sunec.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/jre/lib/ext/sunjce_provider.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/jre/lib/ext/sunpkcs11.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/jre/lib/ext/zipfs.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/jre/lib/jce.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/jre/lib/jfr.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/jre/lib/jsse.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/jre/lib/management-agent.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/jre/lib/resources.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/jre/lib/rt.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/lib/dt.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/lib/jconsole.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/lib/sa-jdi.jar:/Users/miloszm/openjdk/jdk8u265-b01/Contents/Home/lib/tools.jar:/Users/miloszm/proj/epsmi/target/scala-2.13/classes:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/qos/logback/logback-classic/1.2.3/logback-classic-1.2.3.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/qos/logback/logback-core/1.2.3/logback-core-1.2.3.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-annotations/2.10.5/jackson-annotations-2.10.5.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-core/2.10.5/jackson-core-2.10.5.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-databind/2.10.5/jackson-databind-2.10.5.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/fasterxml/jackson/datatype/jackson-datatype-jdk8/2.10.5/jackson-datatype-jdk8-2.10.5.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/fasterxml/jackson/datatype/jackson-datatype-jsr310/2.10.5/jackson-datatype-jsr310-2.10.5.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/ujson_2.13/0.8.0/ujson_2.13-0.8.0.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/upack_2.13/0.8.0/upack_2.13-0.8.0.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/upickle-core_2.13/0.8.0/upickle-core_2.13-0.8.0.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/upickle-implicits_2.13/0.8.0/upickle-implicits_2.13-0.8.0.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/upickle_2.13/0.8.0/upickle_2.13-0.8.0.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/typesafe/akka/akka-actor-typed_2.13/2.6.9/akka-actor-typed_2.13-2.6.9.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/typesafe/akka/akka-actor_2.13/2.6.9/akka-actor_2.13-2.6.9.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/typesafe/akka/akka-http-core_2.13/10.1.12/akka-http-core_2.13-10.1.12.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/typesafe/akka/akka-http-spray-json_2.13/10.1.12/akka-http-spray-json_2.13-10.1.12.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/typesafe/akka/akka-http_2.13/10.1.12/akka-http_2.13-10.1.12.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/typesafe/akka/akka-parsing_2.13/10.1.12/akka-parsing_2.13-10.1.12.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/typesafe/akka/akka-protobuf-v3_2.13/2.6.9/akka-protobuf-v3_2.13-2.6.9.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/typesafe/akka/akka-slf4j_2.13/2.6.9/akka-slf4j_2.13-2.6.9.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/typesafe/akka/akka-stream_2.13/2.6.9/akka-stream_2.13-2.6.9.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/typesafe/play/play-functional_2.13/2.9.1/play-functional_2.13-2.9.1.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/typesafe/play/play-json_2.13/2.9.1/play-json_2.13-2.9.1.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/typesafe/config/1.4.0/config-1.4.0.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/typesafe/ssl-config-core_2.13/0.4.2/ssl-config-core_2.13-0.4.2.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/spray/spray-json_2.13/1.3.5/spray-json_2.13-1.3.5.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/bitcoin-s/bitcoin-s-app-commons_2.13/0.4.0/bitcoin-s-app-commons_2.13-0.4.0.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/bitcoin-s/bitcoin-s-bitcoind-rpc_2.13/0.4.0/bitcoin-s-bitcoind-rpc_2.13-0.4.0.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/bitcoin-s/bitcoin-s-core_2.13/0.4.0/bitcoin-s-core_2.13-0.4.0.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/bitcoin-s/bitcoin-s-crypto_2.13/0.4.0/bitcoin-s-crypto_2.13-0.4.0.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/bitcoin-s/bitcoin-s-secp256k1jni/0.4.0/bitcoin-s-secp256k1jni-0.4.0.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/bouncycastle/bcprov-jdk15on/1.66/bcprov-jdk15on-1.66.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/clapper/grizzled-slf4j_2.13/1.3.4/grizzled-slf4j_2.13-1.3.4.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/reactivestreams/reactive-streams/1.0.3/reactive-streams-1.0.3.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-collection-compat_2.13/2.0.0/scala-collection-compat_2.13-2.0.0.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-java8-compat_2.13/0.9.0/scala-java8-compat_2.13-0.9.0.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-parser-combinators_2.13/1.1.2/scala-parser-combinators_2.13-1.1.2.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.1/scala-library-2.13.1.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-reflect/2.13.1/scala-reflect-2.13.1.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scijava/native-lib-loader/2.3.4/native-lib-loader-2.3.4.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scodec/scodec-bits_2.13/1.1.20/scodec-bits_2.13-1.1.20.jar:/Users/miloszm/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/slf4j/slf4j-api/1.7.30/slf4j-api-1.7.30.jar:/Users/miloszm/proj/epsmi/lib/jsonrpc4j-1.5.3.jar com.mhm.main.Main