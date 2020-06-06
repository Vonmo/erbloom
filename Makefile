DOCKER = $(shell which docker)
ifeq ($(DOCKER),)
$(error "Docker not available on this system")
endif

DOCKER_COMPOSE = UID=`id -u` GID=`id -g` $(shell which docker-compose)
ifeq ($(DOCKER_COMPOSE),)
$(error "DockerCompose not available on this system")
endif

# use to override vars for your platform
ifeq (env.mk,$(wildcard env.mk))
	include env.mk
endif

.PHONY: deps doc test

all: build_imgs up tests rel

build_imgs:
	@echo "Update docker images..."
	@${DOCKER_COMPOSE} build

up:
	@${DOCKER_COMPOSE} up -d

down:
	@${DOCKER_COMPOSE} down

tests:
	@echo "Testing..."
	@${DOCKER_COMPOSE} exec test bash -c "cd /project && make -f docker.mk tests"

test: tests

rel:
	@echo "Build release..."
	@${DOCKER_COMPOSE} exec test bash -c "cd /project && make -f docker.mk prod"

doc:
	@echo "Generate docs..."
	@${DOCKER_COMPOSE} exec test bash -c "cd /project && make -f docker.mk doc"

lint:
	@echo "Lint..."
	@${DOCKER_COMPOSE} exec test bash -c "cd /project && make -f docker.mk lint"

xref:
	@echo "Xref analysis..."
	@${DOCKER_COMPOSE} exec test bash -c "cd /project && make -f docker.mk xref"

dialyzer:
	@echo "Dialyzer..."
	@${DOCKER_COMPOSE} exec test bash -c "cd /project && make -f docker.mk dialyzer"