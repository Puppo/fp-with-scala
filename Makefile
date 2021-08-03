install:
	./.setup.sh

start:
	./.start.sh

run:
	docker exec -ti fp_with_scala_fp_dev sbt run