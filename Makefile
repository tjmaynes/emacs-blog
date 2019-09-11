EMACS_INSTALL_DIRECTORY = $(PWD)
EMACS_VERSION           = 26.2
EMACS                   = emacs
BLOG_DIRECTORY          = $(PWD)
BLOG_CONFIG             = $(BLOG_DIRECTORY)/config.json
BLOG_PUBLISH_SCRIPT     = $(BLOG_DIRECTORY)/scripts/publish.el
BLOG_BUILD_DIRECTORY    = build
PORT                    = 4000

install_dependencies:
	chmod +x ./scripts/install_dependencies.sh
	./scripts/install_dependencies.sh $(EMACS_VERSION) $(EMACS_INSTALL_DIRECTORY)

build-blog:
	which $(EMACS) && \
	BLOG_CONFIG=$(BLOG_CONFIG) \
	BLOG_DIRECTORY=$(BLOG_DIRECTORY) \
	BLOG_BUILD_DIRECTORY=$(BLOG_BUILD_DIRECTORY) \
	$(EMACS) \
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

publish-blog: build-blog

clean:
	rm -rf $(BLOG_BUILD_DIRECTORY) .timestamps
