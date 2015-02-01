package org.bosik.diacomp.core.services.sync;

import java.util.HashMap;
import java.util.Map;
import java.util.NavigableMap;
import java.util.TreeMap;
import org.bosik.diacomp.core.services.ObjectService;

public class MemoryMerkleTree extends TreeMap<String, String> implements MerkleTree
{
	private static final long	serialVersionUID	= 4901379605259573068L;

	@Override
	public String getHash(String prefix)
	{
		return get(prefix);
	}

	@Override
	public Map<String, String> getHashChildren(String prefix)
	{
		NavigableMap<String, String> allChildren = subMap(prefix + "0", true, prefix + "f", true);
		Map<String, String> directChildren = new HashMap<String, String>();

		for (java.util.Map.Entry<String, String> entry : allChildren.entrySet())
		{
			if (prefix.length() >= ObjectService.ID_PREFIX_SIZE || entry.getKey().length() == prefix.length() + 1)
			{
				directChildren.put(entry.getKey(), entry.getValue());
			}
		}

		return directChildren;
	}
}
