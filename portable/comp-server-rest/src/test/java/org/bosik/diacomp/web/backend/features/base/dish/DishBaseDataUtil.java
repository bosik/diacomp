/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.web.backend.features.base.dish;

import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.dishbase.DishItem;
import org.bosik.merklesync.Versioned;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class DishBaseDataUtil
{
	public static List<Versioned<DishItem>> buildDemoData()
	{
		return new ArrayList<Versioned<DishItem>>()
		{{
			add(new Versioned<DishItem>()
			{{
				setId("1");
				setTimeStamp(new Date());
				setHash("hash");
				setVersion(13);
				setDeleted(false);
				setData(new DishItem()
				{{
					setName("Apple pie");
					setMass(165);
					getContent().add(new FoodMassed()
					{{
						setName("Apple");
						setRelProts(0.2);
						setRelFats(0.1);
						setRelCarbs(11.2);
						setRelValue(40);
						setMass(2000);
					}});
				}});
			}});
			add(new Versioned<DishItem>()
			{{
				setId("2");
				setTimeStamp(new Date());
				setHash("hash");
				setVersion(13);
				setDeleted(false);
				setData(new DishItem()
				{{
					setName("Banana");
					setMass(120);
				}});
			}});
		}};
	}
}
