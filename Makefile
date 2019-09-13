BLOG_DIRECTORY              = $(PWD)
BLOG_BUILD_DIRECTORY        = build
BLOG_BUILD_PUBLIC_DIRECTORY = $(BLOG_BUILD_DIRECTORY)/public
LATEX_BUILD_DESTINATION     = $(BLOG_BUILD_PUBLIC_DIRECTORY)/documents
PORT                        = 4000
IMAGE_NAME                  = blog-builder
REGISTRY_USERNAME           = tjmaynes
REGISTRY_PASSWORD           ?= ""
TAG                         = latest

check_emacs_version:
	emacs \
	--no-init-file \
	--version

build_blog:
	BLOG_DIRECTORY=$(BLOG_DIRECTORY) \
	BLOG_CONFIG=$(BLOG_DIRECTORY)/config.json \
	BLOG_BUILD_DIRECTORY=$(BLOG_BUILD_DIRECTORY) \
	emacs \
	--batch \
	--no-init-file \
	--no-site-file \
	--load $(BLOG_DIRECTORY)/scripts/build_blog.el

check_latex_version:
	xelatex --version

compile_latex_file:
	(mkdir -p $(LATEX_BUILD_DESTINATION) || true) && \
	cd tex && xelatex \
	-output-directory=../$(LATEX_BUILD_DESTINATION) \
	$(LATEX_FILE).tex && \
	rm -f ../$(LATEX_BUILD_DESTINATION)/$(LATEX_FILE).log && \
	rm -f ../$(LATEX_BUILD_DESTINATION)/$(LATEX_FILE).aux && \
	rm -f ../$(LATEX_BUILD_DESTINATION)/$(LATEX_FILE).out

compile_cv:
	make compile_latex_file \
	LATEX_FILE=cv

compile_resume:
	make compile_latex_file \
	LATEX_FILE=resume

compile_latex_files: compile_cv compile_resume

check_versions: check_emacs_version check_latex_version

publish_blog: check_versions build_blog compile_latex_files

edit_blog: clean publish_blog
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
