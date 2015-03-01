package org.bosik.diacomp.web.backend.common;

import java.util.HashMap;
import java.util.Map;
import org.bosik.diacomp.core.services.sync.MerkleTree;
import org.springframework.stereotype.Service;

@Service
//@Scope(value = "session", proxyMode = ScopedProxyMode.TARGET_CLASS)
public class CachedHashTree
{
	public enum TreeType {
		DIARY, FOODBASE, DISHBASE
	}

	private class UserTree
	{
		public UserTree()
		{
		}

		MerkleTree	diaryTree;
		MerkleTree	foodTree;
		MerkleTree	dishTree;
	}

	private Map<Integer, UserTree>	trees	= new HashMap<Integer, UserTree>();

	public MerkleTree getTree(int userId, TreeType type)
	{
		synchronized (trees)
		{
			UserTree userTree = trees.get(userId);
			if (userTree == null)
			{
				return null;
			}
			else
			{
				switch (type)
				{
					case DIARY:
						return userTree.diaryTree;
					case FOODBASE:
						return userTree.foodTree;
					case DISHBASE:
						return userTree.dishTree;
					default:
						throw new IllegalArgumentException("Invalid tree type: " + type);
				}
			}
		}
	}

	public void setTree(int userId, TreeType type, MerkleTree tree)
	{
		synchronized (trees)
		{
			UserTree userTree = trees.get(userId);
			if (userTree == null)
			{
				userTree = new UserTree();
				trees.put(userId, userTree);
			}

			switch (type)
			{
				case DIARY:
					userTree.diaryTree = tree;
					break;
				case FOODBASE:
					userTree.foodTree = tree;
					break;
				case DISHBASE:
					userTree.dishTree = tree;
					break;
				default:
					throw new IllegalArgumentException("Invalid tree type: " + type);
			}
		}
	}
}
