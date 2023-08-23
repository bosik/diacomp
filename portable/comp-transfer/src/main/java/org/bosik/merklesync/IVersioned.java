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

import java.util.Comparator;
import java.util.Date;

public interface IVersioned<T>
{
	Comparator<IVersioned<?>> COMPARATOR_GUID = Comparator.comparing(IVersioned::getId);

	String getId();

	void setId(String id);

	Date getTimeStamp();

	void setTimeStamp(Date timeStamp);

	String getHash();

	void setHash(String hash);

	int getVersion();

	void setVersion(int version);

	boolean isDeleted();

	void setDeleted(boolean deleted);

	T getData();

	void setData(T data);
}
