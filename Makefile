BLOG_DIRECTORY          = $(PWD)
BLOG_CONFIG             = $(BLOG_DIRECTORY)/config.json
BLOG_PUBLISH_SCRIPT     = $(BLOG_DIRECTORY)/scripts/publish.el
BLOG_BUILD_DIRECTORY    = build
PORT                    = 4000

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

edit_blog: clean build_blog build_cv
	(docker rm -f nginx-blog || true) && docker run --name nginx-blog \
	--publish $(PORT):80 \
	--publish 443:443 \
	--volume $(BLOG_DIRECTORY)/$(BLOG_BUILD_DIRECTORY):/usr/share/nginx/html \
	nginx:1.15.7

check_latex_version:
	xelatex --version

build_cv:
	xelatex \
	-output-directory=$(BLOG_BUILD_DIRECTORY)/cv \
	pages/cv/cv.tex && \
	rm -f $(BLOG_BUILD_DIRECTORY)/cv/cv.{aux,log,out}

check_versions: check_emacs_version check_latex_version

publish_blog: check_versions build_blog build_cv

clean:
	rm -rf $(BLOG_BUILD_DIRECTORY) .timestamps
