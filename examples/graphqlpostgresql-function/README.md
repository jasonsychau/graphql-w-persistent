# graphqlpostgresql-function

### AWS Lambda Instructions

- update `qqldb_schema.json` file (If you rename this file, you should rename the file path in `src/Lib.hs`)
- build project `stack build --copy-bins`
- copy schema file to build folder `cp gqldb_schema.json .stack-work/docker/_home/.local/bin`
- move dependencies to build folder `mv libpcre.so.3 libpq.so.5 .stack-work/docker/_home/.local/bin`
- create s3 bucket on aws for this function
- upload files to aws and record CloudFormation template `aws cloudformation package --template-file template.yaml --s3-bucket YOUR_BUCKET_NAME > deployment_stack.yaml` (a new file is added to your bucket when you run this command, so you may wish to delete previous files when a new one is made)
- format template (remove the first line from `deployment_stack.yaml`)
- deploy CloudFormation stack `aws cloudformation deploy --stack-name "STACK_NAME" --region REGION --capabilities CAPABILITY_IAM --template-file deployment_stack.yaml`
- test lambda function `aws lambda invoke --function-name FUNCTION_NAME --region REGION --payload '{"query":"{pet{taxonomy{name}}}","variables":""}' output.txt` (You can find the function name on the AWS Lambda console. It is prepended with your CloudFormation stack name and Resource name defined in template.yaml)

