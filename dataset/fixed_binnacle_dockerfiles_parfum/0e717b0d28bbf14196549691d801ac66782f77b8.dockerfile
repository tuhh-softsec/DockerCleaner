#
#  Build an image with node.js and angular-cli and copy the project data into it.
#  Runs NG Live Development Server when the container starts.
#
#  Exposed ports:
#    4200   - app
#    49152  - live reload
#
#  ##################### INFOS ######################
#    - App path: /home/app/angular
#    - User for the execution: app
#
#    - URL: http://localhost:4200
#  ##################### INFOS ######################
#
#
#
#  ##################### DOCKER #####################
#  BUILD
#  docker build -t angular2/example-transform-jsobject-to-specific-class:1.0.0 .
#
#  RUN (add -d parameter to start a container in detached mode)
#  docker run -P -it --rm -p 4200:4200 --name angular2-example-transform-jsobject-to-specific-class angular2/example-transform-jsobject-to-specific-class:1.0.0
#
#  STOP
#  docker stop angular2-example-transform-jsobject-to-specific-class
#
#  REMOVE CONTAINER AND IMAGE
#  docker rm -f angular2-example-transform-jsobject-to-specific-class ; docker rmi -f angular2/example-transform-jsobject-to-specific-class:1.0.0
#
#  LOGIN INTO CONTAINER
#  docker exec -i -t angular2-example-transform-jsobject-to-specific-class /bin/bash
#
#  COPY FILE FROM CONTAINER TO HOST
#  docker cp angular2-example-transform-jsobject-to-specific-class:/home/app/angular/package.json ./package.json
#  ##################### DOCKER #####################
#
#  ********************** INIT **********************
#  build file based on Node.js 7.5.0
FROM node:7.5.0
MAINTAINER "Markus Eschenbach <mail@blogging-it.com>"
#  ********* DEFINE ENVIRONMENT VARIABLES ***********
ENV APP_NAME="\"angular\""
ENV APP_USER="\"app\""
ENV NPM_LOG_LEVEL="\"warn\""
ENV HOME="/home/$APP_USER"
ENV APP_DIR="$HOME/$APP_NAME"
#  ****************** ADD APP USER ******************
RUN useradd --user-group --create-home --shell /bin/false $APP_USER
#  ************* INSTALL ANGULAR CLI ****************
RUN npm install @angular/cli@latest $NPM_LOG_LEVEL --global --loglevel
#  **************** INSTALL YARN ********************
RUN npm install yarn@latest $NPM_LOG_LEVEL --global --loglevel
#  ********** SET GLOBAL PACKAGE MANGER *************
RUN ng set --global packageManager=yarn
#  *************** SET WORKING DIR ******************
WORKDIR $APP_DIR
#  ************* COPY MANIFEST INFOS ****************
COPY package.json $APP_DIR/package.json
#  *********** INSTALL DEPENDENCIES *****************
#  RUN npm install --loglevel $NPM_LOG_LEVEL && npm cache clean
RUN yarn \
 && npm cache clean
#  ************ COPY PROJECT FILES ******************
COPY . $APP_DIR
#  ************** SET USER RIGHTS *******************
RUN chown -R $APP_USER:$APP_USER $HOME/*
USER $APP_USER
#  ******************** EXPOSE *********************
EXPOSE 4200/tcp 49152/tcp
#  ***************** START SERVER ******************
CMD ["ng", "serve", "--host=0.0.0.0"]
