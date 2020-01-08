BLOG_DIRECTORY              = $(PWD)
BLOG_BUILD_DIRECTORY_NAME   = build
BLOG_BUILD_DIRECTORY        = $(PWD)/$(BLOG_BUILD_DIRECTORY_NAME)
BLOG_BUILD_PUBLIC_DIRECTORY = $(BLOG_BUILD_DIRECTORY)/public
BLOG_CONFIG                 = $(BLOG_DIRECTORY)/config.json
IMAGE_NAME                  = blog-builder
PORT                        = 4000
REGISTRY_USERNAME           = tjmaynes
REGISTRY_PASSWORD           ?= ""
TAG                         = latest
TARGET_BRANCH               = gh-pages

check_emacs_version:
	emacs \
	--no-init-file \
	--version

check_latex_version:
	xelatex --version

check_versions: check_emacs_version check_latex_version

build_blog:
	BLOG_DIRECTORY=$(BLOG_DIRECTORY) \
	BLOG_CONFIG=$(BLOG_CONFIG) \
	BLOG_BUILD_DIRECTORY=$(BLOG_BUILD_DIRECTORY_NAME) \
	emacs \
	--batch \
	--no-init-file \
	--no-site-file \
	--load $(BLOG_DIRECTORY)/scripts/build_blog.el

copy_static_files:
	chmod +x ./scripts/copy_static_files.sh
	./scripts/copy_static_files.sh \
	$(BLOG_BUILD_DIRECTORY_NAME)

build_latex_files:
	chmod +x ./scripts/build_latex_files.sh
	./scripts/build_latex_files.sh \
	$(BLOG_BUILD_DIRECTORY_NAME)

publish_blog: check_versions build_blog copy_static_files build_latex_files

deploy_blog:
	chmod +x ./scripts/deploy_blog.sh
	./scripts/deploy_blog.sh \
	$(GIT_USERNAME) \
	$(GIT_EMAIL) \
	$(GIT_COMMIT_SHA) \
	$(TARGET_BRANCH) \
	$(BLOG_BUILD_DIRECTORY_NAME)

preview_blog: build_blog
	chmod +x ./scripts/edit_blog.sh
	./scripts/edit_blog.sh \
	$(PORT) \
	$(BLOG_BUILD_DIRECTORY)

build_image:
	chmod +x ./scripts/build_image.sh
	./scripts/build_image.sh $(REGISTRY_USERNAME) $(IMAGE_NAME) $(TAG)

debug_image: clean
	chmod +x ./scripts/debug_image.sh
	./scripts/debug_image.sh \
	$(REGISTRY_USERNAME) \
	$(IMAGE_NAME) \
	$(TAG) \
	$(BLOG_DIRECTORY)

push_image:
	chmod +x ./scripts/push_image.sh
	./scripts/push_image.sh \
	$(REGISTRY_USERNAME) \
	$(REGISTRY_PASSWORD) \
	$(IMAGE_NAME) \
	$(TAG)

clean:
	rm -rf $(BLOG_BUILD_DIRECTORY) .timestamps
