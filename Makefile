BLOG_DIRECTORY          = $(PWD)
BLOG_CONFIG             = $(BLOG_DIRECTORY)/config.json
BLOG_PUBLISH_SCRIPT     = $(BLOG_DIRECTORY)/scripts/publish.el
BLOG_BUILD_DIRECTORY    = build
PORT                    = 4000
IMAGE_NAME              = blog-builder
REGISTRY_USERNAME       = tjmaynes
REGISTRY_PASSWORD       ?= ""
TAG                     = latest

check_emacs_version:
	emacs \
	--no-init-file \
	--version

build_blog:
	BLOG_CONFIG=$(BLOG_CONFIG) \
	BLOG_DIRECTORY=$(BLOG_DIRECTORY) \
	BLOG_BUILD_DIRECTORY=$(BLOG_BUILD_DIRECTORY) \
	emacs \
	--batch \
	--no-init-file \
	--no-site-file \
	--load $(BLOG_PUBLISH_SCRIPT)

check_latex_version:
	xelatex --version

build_cv:
	xelatex \
	-output-directory=$(BLOG_BUILD_DIRECTORY)/cv \
	pages/cv/cv.tex && \
	rm -f $(BLOG_BUILD_DIRECTORY)/cv/cv.log && \
	rm -f $(BLOG_BUILD_DIRECTORY)/cv/cv.aux && \
	rm -f $(BLOG_BUILD_DIRECTORY)/cv/cv.out

check_versions: check_emacs_version check_latex_version

publish_blog: check_versions build_blog build_cv

edit_blog: clean build_blog build_cv
	(docker rm -f nginx-blog || true) && docker run --name nginx-blog \
	--publish $(PORT):80 \
	--publish 443:443 \
	--volume $(BLOG_DIRECTORY)/$(BLOG_BUILD_DIRECTORY):/usr/share/nginx/html \
	nginx:1.15.7

build_image:
	chmod +x ./scripts/build_image.sh
	./scripts/build_image.sh $(REGISTRY_USERNAME) $(IMAGE_NAME) $(TAG)

test_image: clean
	docker run --rm -it \
	--workdir /src \
	--volume $(BLOG_DIRECTORY):/src \
	$(REGISTRY_USERNAME)/$(IMAGE_NAME):$(TAG) \
	publish_blog BLOG_DIRECTORY=/src/

push_image:
	chmod +x ./scripts/push_image.sh
	./scripts/push_image.sh \
	$(REGISTRY_USERNAME) \
	$(REGISTRY_PASSWORD) \
	$(IMAGE_NAME) \
	$(TAG)

clean:
	rm -rf $(BLOG_BUILD_DIRECTORY) .timestamps
