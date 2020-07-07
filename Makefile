BLOG_DIRECTORY              = $(realpath $(PWD))
BLOG_PUBLISHING_DIRECTORY   = $(BLOG_DIRECTORY)/build
CAREER_FILES_REPO           = "https://github.com/tjmaynes/career"
IMAGE_NAME                  = blog-builder
PORT                        = 80
REGISTRY_USERNAME           = tjmaynes2
REGISTRY_PASSWORD           ?= ""
TAG                         = latest
TARGET_BRANCH               = gh-pages
REPO                        = git@github.com:tjmaynes/blog.git

define download_release_from_repo
(mkdir -p ${2} || true) && git clone --single-branch --branch release ${1} ${2}
rm -rf ${2}/.git
endef

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
	--load $(BLOG_DIRECTORY)/scripts/generate_blog.el

get_career_files:
	$(call download_release_from_repo,$(CAREER_FILES_REPO),$(BLOG_PUBLISHING_DIRECTORY)/career)

publish_blog: check_emacs_version build_blog get_career_files

deploy_artifact:
	chmod +x $(BLOG_DIRECTORY)/scripts/deploy_artifact.sh
	$(BLOG_DIRECTORY)/scripts/deploy_artifact.sh \
	$(GIT_USERNAME) \
	$(GIT_EMAIL) \
	$(GIT_COMMIT_SHA) \
	$(TARGET_BRANCH) \
	$(BLOG_PUBLISHING_DIRECTORY) \
	$(REPO)

preview_blog: build_blog
	chmod +x $(BLOG_DIRECTORY)/scripts/edit_blog.sh
	$(BLOG_DIRECTORY)/scripts/edit_blog.sh \
	$(PORT) \
	$(BLOG_PUBLISHING_DIRECTORY)

build_image:
	chmod +x $(BLOG_DIRECTORY)/scripts/build_image.sh
	$(BLOG_DIRECTORY)/scripts/build_image.sh $(REGISTRY_USERNAME) $(IMAGE_NAME) $(TAG)

debug_image: clean
	chmod +x $(BLOG_DIRECTORY)/scripts/debug_image.sh
	$(BLOG_DIRECTORY)/scripts/debug_image.sh \
	$(REGISTRY_USERNAME) \
	$(IMAGE_NAME) \
	$(TAG) \
	$(BLOG_DIRECTORY)

push_image:
	chmod +x $(BLOG_DIRECTORY)/scripts/push_image.sh
	$(BLOG_DIRECTORY)/scripts/push_image.sh \
	$(REGISTRY_USERNAME) \
	$(REGISTRY_PASSWORD) \
	$(IMAGE_NAME) \
	$(TAG)

clean:
	rm -rf $(BLOG_PUBLISHING_DIRECTORY) .timestamps
