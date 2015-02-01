package org.bosik.diacomp.core.services.sync;

import java.util.Map;

public interface MerkleTree
{
	/**
	 * Returns hash for the specified prefix
	 * 
	 * @param prefix
	 * @return
	 */
	String getHash(String prefix);

	/**
	 * Returns key-values pairs for direct children
	 * 
	 * @param prefix
	 * @return
	 */
	Map<String, String> getHashChildren(String prefix);
}
