#Interesting problem: the slim version caused Future.get to return nil
#desipe the function clearly returning a value. Something was taken out
#of the jdk distribution that was required and a compilation error was
#not thrown!
#FROM openjdk:11.0.13-jre-slim

FROM openjdk:11.0.13

RUN mkdir -p /usr/local/app/data
COPY target/uberjar/arc-solver2-*-standalone.jar /usr/local/app/arc-solver2.jar

RUN adduser --disabled-password --gecos "" arc
USER arc:arc
WORKDIR /usr/local/app

ENTRYPOINT ["java", "-jar", "arc-solver2.jar"]
CMD ["greedy-limited-bfs"]