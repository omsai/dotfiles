# Generate each IdentityFile.
IDFILES = $(sort $(shell sed -n -E 's/[[:space:]]*IdentityFile (.+)/\1/p' config))

.PHONY : identity-files
identity-files : $(IDFILES)
$(IDFILES) : % :
	ssh-keygen -t rsa -N "" -f $@

.PHONY : clean
clean :
	rm -f $(IDFILES) $(IDFILES:=.pub)
