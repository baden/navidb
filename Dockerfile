#FROM voidlock/erlang:R16B03-1

FROM erlang:18
#FROM baden/erlang:R16B02

MAINTAINER Denys Batrak <baden.i.ua@gmail.com>
#

ADD . /home/user/app
WORKDIR /home/user/app

# RUN rm -Rf deps && make deps && make compile

CMD ["/bin/bash", "entrypoint.sh"]
