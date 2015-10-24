/*
 * MerkleSync - Data synchronization routine based on Merkle hash trees
 * Copyright (C) 2013 Nikita Bosik
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.bosik.merklesync;

import java.util.Map;

public interface MerkleTree
{
	/**
	 * Returns hash for the specified prefix
	 * 
	 * @param prefix
	 *            Must be 0..ID_PREFIX_SIZE chars long
	 * @return Hash for specified ID prefix, or null if hash not found
	 */
	String getHash(String prefix);

	/**
	 * Returns key-values pairs for direct children
	 * 
	 * @param prefix
	 *            Must be 0..ID_PREFIX_SIZE chars long
	 * @return Map (prefix + one_char, hash) if prefix is shorter than ID_PREFIX_SIZE; (id, hash) otherwise
	 */
	Map<String, String> getHashChildren(String prefix);
}
