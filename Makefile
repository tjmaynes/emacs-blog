BLOG_DIRECTORY          = $(PWD)
EMACS_INSTALL_DIRECTORY = $(BLOG_DIRECTORY)
EMACS_VERSION           = 26.2
BLOG_CONFIG             = $(BLOG_DIRECTORY)/config.json
BLOG_PUBLISH_SCRIPT     = $(BLOG_DIRECTORY)/scripts/publish.el
BLOG_BUILD_DIRECTORY    = build
PORT                    = 4000

install_dependencies:
	chmod +x ./scripts/install_dependencies.sh
	./scripts/install_dependencies.sh $(EMACS_VERSION) $(EMACS_INSTALL_DIRECTORY)

check-emacs-version:
	emacs \
	--no-init-file \
	--version

build-blog:
	BLOG_CONFIG=$(BLOG_CONFIG) \
	BLOG_DIRECTORY=$(BLOG_DIRECTORY) \
	BLOG_BUILD_DIRECTORY=$(BLOG_BUILD_DIRECTORY) \
	emacs \
	--batch \
	--no-init-file \
	--no-site-file \
	--load $(BLOG_PUBLISH_SCRIPT)

edit-blog: clean build-blog
	(docker rm -f nginx-blog || true) && docker run --name nginx-blog \
	--publish $(PORT):80 \
	--publish 443:443 \
	--volume $(BLOG_DIRECTORY)/$(BLOG_BUILD_DIRECTORY):/usr/share/nginx/html \
	nginx:1.15.7

publish-blog: check-emacs-version build-blog

deploy-blog:
	chmod +x ./scripts/deploy.sh
	./scripts/deploy.sh \
	$(BLOG_PUBLISH_DIRECTORY)

clean:
	rm -rf $(BLOG_BUILD_DIRECTORY) .timestamps
