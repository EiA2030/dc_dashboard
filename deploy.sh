docker login -u ${DOCKERHUB_USERNAME} -p ${DOCKERHUB_PASSWORD}
sudo mkdir -p /var/lib/app/${REPO_NAME}
sudo chown -R ${SSH_USER}:${SSH_USER} /var/lib/app/${REPO_NAME}
mv docker-compose.yml /var/lib/app/${REPO_NAME}
docker-compose -f /var/lib/app/${REPO_NAME}/docker-compose.yml down
docker-compose -f /var/lib/app/${REPO_NAME}/docker-compose.yml pull
docker-compose -f /var/lib/app/${REPO_NAME}/docker-compose.yml up -d
docker image prune -a -f