BLOG_DIRECTORY              = $(PWD)
BLOG_PUBLISHING_DIRECTORY   = $(BLOG_DIRECTORY)/build
IMAGE_NAME                  = blog-builder
PORT                        = 4000
REGISTRY_USERNAME           = tjmaynes
REGISTRY_PASSWORD           ?= ""
TAG                         = latest
TARGET_BRANCH               = gh-pages
REPO                        = git@github.com:tjmaynes/blog.git

check_emacs_version:
	emacs \
	--no-init-file \
	--version

build_blog:
	BLOG_DIRECTORY=$(BLOG_DIRECTORY) \
	BLOG_PUBLISHING_DIRECTORY=$(BLOG_PUBLISHING_DIRECTORY) \
	emacs \
	--batch \
	--no-init-file \
	--no-site-file \
	--load $(BLOG_DIRECTORY)/scripts/build_blog.el

get_career_files:
	chmod +x ./scripts/get_career_files.sh
	./scripts/get_career_files.sh \
	$(BLOG_PUBLISHING_DIRECTORY)

publish_blog: check_emacs_version build_blog get_career_files

deploy_artifact:
	chmod +x ./scripts/deploy_artifact.sh
	./scripts/deploy_artifact.sh \
	$(GIT_USERNAME) \
	$(GIT_EMAIL) \
	$(GIT_COMMIT_SHA) \
	$(TARGET_BRANCH) \
	$(BLOG_PUBLISHING_DIRECTORY) \
	$(REPO)

preview_blog: build_blog
	chmod +x ./scripts/edit_blog.sh
	./scripts/edit_blog.sh \
	$(PORT) \
	$(BLOG_PUBLISHING_DIRECTORY)

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
	rm -rf $(BLOG_PUBLISHING_DIRECTORY) .timestamps
