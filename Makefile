.PHONY:= build tests docs deploy login clean

IMAGE:=usgseros/lcmap-gaia

.DEFAULT_GOAL := build
VERSION    := `./bin/version`
IMAGE      := eroslab.cr.usgs.gov:4567/lcmap/gaia
BRANCH     := $(or $(CI_COMMIT_REF_NAME),`git rev-parse --abbrev-ref HEAD`)
BRANCH     := $(shell echo $(BRANCH) | tr / -)
SHORT_HASH := `git rev-parse --short HEAD`
TAG        := $(IMAGE):$(BRANCH)-$(VERSION)-$(SHORT_HASH)


# LCMAP Standard Makefile targets.  Do not remove.

build:
	@docker build -t $(TAG) \
                      --rm=true \
                      --compress $(PWD)

tests:  
	@docker run --rm \
                    -e Xms=4352m \
                    -e Xmx=4352m \
                    --entrypoint /app/bin/lein-test-entrypoint.sh $(TAG)	

docs:
	@lein codox

deploy: login
	docker push $(TAG)

# Extra Makefile targets. Edit at will.

login:
	@$(if $(and $(CI_REGISTRY_USER), $(CI_REGISTRY_PASSWORD)), \
          docker login  -u $(CI_REGISTRY_USER) \
                        -p $(CI_REGISTRY_PASSWORD) \
                         $(CI_REGISTRY), \
          docker login eroslab.cr.usgs.gov:4567)

clean:
	@lein clean
	@rm -rf docs/
