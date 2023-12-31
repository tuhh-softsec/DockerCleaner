#   models_base
FROM ubuntu:18.04
WORKDIR /home
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
 && apt-get install --no-install-recommends iputils-ping=3:20161105-1ubuntu3 git=1:2.17.1-1ubuntu0.17 -y -q
#   Fetch python3 and Install python3
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
 && apt-get install --no-install-recommends libprotobuf-dev=3.0.0-9.1ubuntu1.1 python3-dev=3.6.7-1~18.04 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 virtualenv=15.1.0+ds-1.1 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libssl-doc=1.1.1-1ubuntu2.1~18.04.21 python3-setuptools=39.0.1-2ubuntu0.1 python3-wheel=0.30.0-0.2ubuntu0.1 -y -q
RUN pip3 install numpy scipy -i https://pypi.tuna.tsinghua.edu.cn/simple/ --trusted-host pypi.tuna.tsinghua.edu.cn
#   Install the MO in the DLDT
ARG DLDT_VER=2018_R4
ARG DLDT_REPO=https://github.com/opencv/dldt.git
ARG MO=/home/dldt/model-optimizer
RUN git clone -b ${DLDT_VER} ${DLDT_REPO} ; cd dldt ; git submodule init ; git submodule update --recursive ; cd ${MO} ; pip3 install -r requirements.txt -i https://pypi.tuna.tsinghua.edu.cn/simple/ --trusted-host pypi.tuna.tsinghua.edu.cn
#   Fetch the models
ARG MODEL_ZOO_VER=2018
ARG MODEL_ZOO_REPO=https://github.com/opencv/open_model_zoo.git
RUN git clone -b ${MODEL_ZOO_VER} ${MODEL_ZOO_REPO}
RUN pip3 install pyyaml requests
#   Download and convert the Mobilenet-SSD model
ARG MO=/home/dldt/model-optimizer
ARG MODELS_DOWNLOAD_DIR=/home/tmp_models
ARG MODEL_ZOO_DIR=/home/video-analytics/models
ARG MODEL_NAME=mobilenet-ssd
ARG DEPLOY_MODEL_NAME=object_detection
ARG DEPLOY_MODEL_VERSION=1
RUN cd /home/open_model_zoo/model_downloader ; python3 downloader.py --name ${MODEL_NAME} -o ${MODELS_DOWNLOAD_DIR} ; mkdir -p ${MODEL_ZOO_DIR}/${DEPLOY_MODEL_NAME}/${DEPLOY_MODEL_VERSION}/FP32 ; cd ${MODEL_ZOO_DIR}/${DEPLOY_MODEL_NAME}/${DEPLOY_MODEL_VERSION}/FP32 ; ${MO}/mo.py --framework caffe --data_type FP32 --input_shape [1,3,300,300] --input data --mean_values data[127.5,127.5,127.5] --scale_values data[127.50223128904757] --output detection_out --input_model ${MODELS_DOWNLOAD_DIR}/object_detection/common/mobilenet-ssd/caffe/${MODEL_NAME}.caffemodel --input_proto ${MODELS_DOWNLOAD_DIR}/object_detection/common/mobilenet-ssd/caffe/${MODEL_NAME}.prototxt
#   Download and convert the face-detection-adas-0001 model
ARG MODEL_NAME=face-detection-adas-0001
ARG OUTPUT=Transportation/object_detection/face/pruned_mobilenet_reduced_ssd_shared_weights/dldt
ARG DEPLOY_MODEL_NAME=face_detection_adas
ARG DEPLOY_MODEL_VERSION=1
RUN cd /home/open_model_zoo/model_downloader ; python3 downloader.py --name ${MODEL_NAME} -o ${MODELS_DOWNLOAD_DIR} ; mkdir -p ${MODEL_ZOO_DIR}/${DEPLOY_MODEL_NAME}/${DEPLOY_MODEL_VERSION}/FP32 ; cd ${MODEL_ZOO_DIR}/${DEPLOY_MODEL_NAME}/${DEPLOY_MODEL_VERSION}/FP32 ; cp ${MODELS_DOWNLOAD_DIR}/${OUTPUT}/* .
#   Download and convert the face-detection-retail-0004 model
ARG MODEL_NAME=face-detection-retail-0004
ARG OUTPUT=Retail/object_detection/face/sqnet1.0modif-ssd/0004/dldt
ARG DEPLOY_MODEL_NAME=face_detection_retail
ARG DEPLOY_MODEL_VERSION=1
RUN cd /home/open_model_zoo/model_downloader ; python3 downloader.py --name ${MODEL_NAME} -o ${MODELS_DOWNLOAD_DIR} ; mkdir -p ${MODEL_ZOO_DIR}/${DEPLOY_MODEL_NAME}/${DEPLOY_MODEL_VERSION}/FP32 ; cd ${MODEL_ZOO_DIR}/${DEPLOY_MODEL_NAME}/${DEPLOY_MODEL_VERSION}/FP32 ; cp ${MODELS_DOWNLOAD_DIR}/${OUTPUT}/* .
#   Download and convert the face-reidentification-retail-0095 model
ARG MODEL_NAME=face-reidentification-retail-0095
ARG OUTPUT=Retail/object_reidentification/face/mobilenet_based/dldt
ARG DEPLOY_MODEL_NAME=face_reidentification
ARG DEPLOY_MODEL_VERSION=1
RUN cd /home/open_model_zoo/model_downloader ; python3 downloader.py --name ${MODEL_NAME} -o ${MODELS_DOWNLOAD_DIR} ; mkdir -p ${MODEL_ZOO_DIR}/${DEPLOY_MODEL_NAME}/${DEPLOY_MODEL_VERSION}/FP32 ; cd ${MODEL_ZOO_DIR}/${DEPLOY_MODEL_NAME}/${DEPLOY_MODEL_VERSION}/FP32 ; cp ${MODELS_DOWNLOAD_DIR}/${OUTPUT}/* .
#   Download and convert the emotions-recognition-retail-0003 model
ARG MODEL_NAME=emotions-recognition-retail-0003
ARG OUTPUT=Retail/object_attributes/emotions_recognition/0003/dldt
ARG DEPLOY_MODEL_NAME=emotion_recognition
ARG DEPLOY_MODEL_VERSION=1
RUN cd /home/open_model_zoo/model_downloader ; python3 downloader.py --name ${MODEL_NAME} -o ${MODELS_DOWNLOAD_DIR} ; mkdir -p ${MODEL_ZOO_DIR}/${DEPLOY_MODEL_NAME}/${DEPLOY_MODEL_VERSION}/FP32 ; cd ${MODEL_ZOO_DIR}/${DEPLOY_MODEL_NAME}/${DEPLOY_MODEL_VERSION}/FP32 ; cp ${MODELS_DOWNLOAD_DIR}/${OUTPUT}/* .
#   Download and convert the landmarks-regression-retail-0009 model
ARG MODEL_NAME=landmarks-regression-retail-0009
ARG OUTPUT=Retail/object_attributes/landmarks_regression/0009/dldt
ARG DEPLOY_MODEL_NAME=landmarks_regression
ARG DEPLOY_MODEL_VERSION=1
RUN cd /home/open_model_zoo/model_downloader ; python3 downloader.py --name ${MODEL_NAME} -o ${MODELS_DOWNLOAD_DIR} ; mkdir -p ${MODEL_ZOO_DIR}/${DEPLOY_MODEL_NAME}/${DEPLOY_MODEL_VERSION}/FP32 ; cd ${MODEL_ZOO_DIR}/${DEPLOY_MODEL_NAME}/${DEPLOY_MODEL_VERSION}/FP32 ; cp ${MODELS_DOWNLOAD_DIR}/${OUTPUT}/* .
RUN rm -rf ${MODELS_DOWNLOAD_DIR} \
 && rm -rf /var/lib/apt/lists/*
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
