pipeline {
    agent any

    stages {
        stage ('Received push') {
            steps  {
                echo 'Begin build'
                sh script: '''
                #!/bin/bash
                sudo rm -rf audacity
                '''
            }
        }
        
        /*
        stage('wxWidgets') {
            steps {
                sh script: '''
                #!/bin/bash
                git clone --recurse-submodules https://github.com/audacity/wxWidgets/
                cd wxWidgets
                mkdir buildgtk
                cd buildgtk
                echo cmake --version
                ../configure --with-cxx=14 --with-gtk=2
                sudo make install
                '''
            }
        }
        */
        
        
        stage('audacity') {
            steps {
                sh script: '''
                #!/bin/bash
                git clone https://github.com/ECSE437-Audacity/audacity.git
                cd audacity
                mkdir build
                cd build
                cmake -DCMAKE_BUILD_TYPE=Release -Daudacity_use+ffmpeg=loaded ..
                make
                '''
            }
        }
        
        stage('test build') {
            steps {
                script {
                    try {
                        sh script: '''
                        #!/bin/bash
                        cd audacity/build/bin/Release
                        mkdir "Portable Settings"
                        ./audacity
                        '''
                    }
                    catch (err) {
                        echo err.getMessage()
                    }
                    echo currentBuild.result
                }
            }
        }
        
        stage('install audacity') {
            steps {
                sh script: '''
                #!/bin/bash
                cd audacity/build
                sudo make install
                '''
            }
        }
    }
        
}
