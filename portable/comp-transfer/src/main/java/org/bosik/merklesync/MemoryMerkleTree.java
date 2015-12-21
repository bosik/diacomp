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

import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

public class MemoryMerkleTree extends TreeMap<String, String> implements MerkleTree
{
	private static final long serialVersionUID = 4901379605259573068L;

	@Override
	public String getHash(String prefix)
	{
		return get(prefix);
	}

	@Override
	public Map<String, String> getHashChildren(final String prefix)
	{
		if (prefix.length() < DataSource.ID_PREFIX_SIZE)
		{
			Map<String, String> directChildren = new HashMap<String, String>();

			for (char c : HashUtils.BYTE_TO_CHAR)
			{
				String key = prefix + c;
				String value = get(key);
				if (value != null)
				{
					directChildren.put(key, value);
				}
			}

			return directChildren;
		}
		else
		{
			return subMap(prefix + "0", true, prefix + "f", true);
		}
	}
}
