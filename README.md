## The Hoisted Content Management System based on Lift

### Getting started.

1. git clone https://github.com/hoisted/hoisted.git
2. git clone https://github.com/lift/cms_site.git
3. cd hoisted
4. ./sbt11 assembly
5. cd ..
6. mkdir resulting_site
7. java -jar ./hoisted/target/hoisted.jar cms_site/site/ resulting_site/
8. All the files from cms_site/* will be transformed by hoisted, and the resulting html files
   will end up in resulting_site/
9. You can then copy those files to your apache or jetty web app folder.


You can browse the [liftweb.net site repository](https://github.com/lift/cms_site/tree/master/site) on github to learn more about the supported syntax.

### Asking for help

The [Lift mailing list](https://groups.google.com/forum/?fromgroups#!forum/liftweb) is the place to ask for help, if the volume of questions is too high, we will open a separate mailing list for Hoisted.




    /*
     * Copyright 2009 WorldWide Conferencing, LLC
     * Licensed under the Apache License, Version 2.0 (the "License");
     * you may not use this file except in compliance with the License.
     * You may obtain a copy of the License at
     *
     *    http://www.apache.org/licenses/LICENSE-2.0
     *
     * Unless required by applicable law or agreed to in writing,
     * software distributed under the License is distributed on an "AS IS" BASIS,
     * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
     * See the License for the specific language governing permissions
     * and limitations under the License.
     */
