version := "0.1.0-SNAPSHOT"

scalaVersion := "3.3.0"

name := "autocomplete-playground"

lazy val scalatestVersion = "3.2.15"
lazy val luceneVersion    = "9.7.0"

libraryDependencies ++= Seq(
  "org.typelevel"            %% "cats-effect"            % "3.5.0",
  "co.fs2"                   %% "fs2-core"               % "3.7.0",
  "co.fs2"                   %% "fs2-io"                 % "3.7.0",
  "ch.qos.logback"            % "logback-classic"        % "1.4.7",
  "io.circe"                 %% "circe-core"             % "0.14.5",
  "io.circe"                 %% "circe-generic"          % "0.14.5",
  "io.circe"                 %% "circe-parser"           % "0.14.5",
  "org.apache.lucene"         % "lucene-core"            % luceneVersion,
  "org.apache.lucene"         % "lucene-suggest"         % luceneVersion,
  "org.apache.lucene"         % "lucene-analysis-common" % luceneVersion,
  "org.apache.lucene"         % "lucene-queryparser"     % luceneVersion,
  "org.apache.lucene"         % "lucene-analysis-icu"    % luceneVersion,
  "commons-io"                % "commons-io"             % "2.11.0",
  "org.apache.commons"        % "commons-lang3"          % "3.12.0",
  "com.github.luben"          % "zstd-jni"               % "1.5.5-4",
  "com.opencsv"               % "opencsv"                % "5.7.1",
  "com.microsoft.onnxruntime" % "onnxruntime_gpu"        % "1.15.1",
  "ai.djl"                    % "api"                    % "0.23.0",
  "org.scalatest"            %% "scalatest"              % "3.2.15" % "test"
)
