#!/usr/bin/env bash
#
# Audacity: A Digital Audio Editor
#

# Enable exit on any error
trap 'echo Sign failed; exit 1' ERR

S3_KEY=""
S3_SECRET=""
FILE_PATH=""

S3_BUCKET="muse-sign"
S3_UNSIGNED_DIR="unsigned"
S3_SIGNED_DIR="signed"

while [[ "$#" -gt 0 ]]; do
    case $1 in
        --s3_key) S3_KEY="$2"; shift ;;
        --s3_secret) S3_SECRET="$2"; shift ;;
        --file_path) FILE_PATH="$2"; shift ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

if [ -z "$S3_KEY" ]; then echo "error: not set S3_KEY"; exit 1; fi
if [ -z "$S3_SECRET" ]; then echo "error: not set S3_SECRET"; exit 1; fi
if [ -z "$FILE_PATH" ]; then echo "error: not set FILE_PATH"; exit 1; fi

FILE_NAME="$(basename "${FILE_PATH}")"
S3_UNSIGNED_URL="s3://$S3_BUCKET/$S3_UNSIGNED_DIR/$FILE_NAME"
S3_SIGNED_URL="s3://$S3_BUCKET/$S3_SIGNED_DIR/$FILE_NAME"
FILE_SIGNED_PATH="${FILE_PATH}_signed"

export AWS_ACCESS_KEY_ID=$S3_KEY
export AWS_SECRET_ACCESS_KEY=$S3_SECRET 
export AWS_DEFAULT_REGION=us-east-1

aws s3 ls s3://$S3_BUCKET

echo "Send file to sign service..."
aws s3 cp $FILE_PATH $S3_UNSIGNED_URL
aws s3 ls s3://$S3_BUCKET/$S3_UNSIGNED_DIR/

# Disable exit on any error
trap '' ERR

signed=-1
for i in 1 2 3 4 5 6 7 8 9; do
    echo "Check sign... $i"
    aws s3 cp $S3_SIGNED_URL $FILE_SIGNED_PATH
    signed=$?
    if [ $signed -eq 0 ]; then break; fi
    if [ $i -eq 9 ]; then
        echo "Sign failed."
        exit 1
    fi
    echo "does not exist is normal, waiting 60 seconds"
    sleep 60
done

echo "Signed file downloaded successfully"

# Enable exit on any error
trap 'echo Sign failed; exit 1' ERR

echo "Delete signed file from service"
aws s3 rm $S3_SIGNED_URL 

echo "Rename original unsigned file"
mv $FILE_PATH "${FILE_PATH}_origin"

echo "Rename signed file to original name"
mv $FILE_SIGNED_PATH $FILE_PATH

echo "Delete original unsigned file"
rm -f "${FILE_PATH}_origin"

echo "All done"
