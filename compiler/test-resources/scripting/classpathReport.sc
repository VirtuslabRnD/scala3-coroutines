#!dist/target/pack/bin/scala -classpath 'dist/target/pack/lib/*'

import java.nio.file.Paths

def main(args: Array[String]): Unit =
  val cwd = Paths.get(".").toAbsolutePath.toString.replace('\\', '/').replaceAll("/$", "")
  printf("cwd: %s\n", cwd)
  printf("classpath: %s\n", sys.props("java.class.path"))

