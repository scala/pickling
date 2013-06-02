/*
 * Copyright 2012 Albert Örwall
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.evactor.model.events

import scala.Array
import java.net.URLEncoder

/**
 * The event class all other event's should inherit.
 * 
 * It's identified by the event type (class name) and id. An event
 * can have clones with different paths.
 * 
 */

trait Event extends Serializable {
    val id: String
    val timestamp: Int
}
