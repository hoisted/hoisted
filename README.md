## The Hoisted Content Management System based on Lift

### Getting started.

1. git clone https://github.com/hoisted/hoisted.git
2. cd hoisted
3. ./sbt11
4. Edit the file  **./src/main/resources/props/default.props** and add **cms.root=/path/to/your/site/files**
5. Your site files should end with cms.xml (and are written in xhtml)

    \> container:start
    
6. Go to http://127.0.0.1


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
