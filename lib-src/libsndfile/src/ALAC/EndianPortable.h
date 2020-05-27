/*
 * Copyright (c) 2011 Apple Inc. All rights reserved.
** Copyright (C) 2013-2014 Erik de Castro Lopo <erikd@mega-nerd.com>
 *
 * @APPLE_APACHE_LICENSE_HEADER_START@
 *
 * Licensed under the Apache License, Version 2.0 (the "License") ;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * @APPLE_APACHE_LICENSE_HEADER_END@
 */

//
//  EndianPortable.h
//
//  Copyright 2011 Apple Inc. All rights reserved.
//

#ifndef _EndianPortable_h
#define _EndianPortable_h

#include <sfendian.h>

#define Swap16NtoB(x)	H2BE_16 (x)
#define Swap16BtoN(x)	BE2H_16 (x)

#define Swap32NtoB(x)	H2BE_32 (x)
#define Swap32BtoN(x)	BE2H_32 (x)

#endif
