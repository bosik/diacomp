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
package org.bosik.diacomp.web.backend.features.diary;

import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.entities.business.diary.DiaryRecord;
import org.bosik.diacomp.core.entities.business.diary.records.BloodRecord;
import org.bosik.diacomp.core.entities.business.diary.records.InsRecord;
import org.bosik.diacomp.core.entities.business.diary.records.MealRecord;
import org.bosik.diacomp.core.entities.business.diary.records.NoteRecord;
import org.bosik.merklesync.Versioned;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public final class DiaryDataUtil
{
	public static List<Versioned<DiaryRecord>> buildDemoData()
	{
		// TODO: improve type bounds
		return new ArrayList<Versioned<DiaryRecord>>()
		{{
			add((Versioned) new Versioned<BloodRecord>()
			{{
				setId("1");
				setTimeStamp(new Date());
				setHash("hash");
				setVersion(13);
				setDeleted(false);
				setData(new BloodRecord()
				{{
					setTime(new Date());
					setValue(5.2);
					setFinger(2);
				}});
			}});
			add((Versioned) new Versioned<InsRecord>()
			{{
				setId("2");
				setTimeStamp(new Date());
				setHash("hash");
				setVersion(13);
				setDeleted(false);
				setData(new InsRecord()
				{{
					setTime(new Date());
					setValue(15.5);
				}});
			}});
			add((Versioned) new Versioned<MealRecord>()
			{{
				setId("3");
				setTimeStamp(new Date());
				setHash("hash");
				setVersion(13);
				setDeleted(false);
				setData(new MealRecord()
				{{
					setTime(new Date());
					setShortMeal(true);
					add(new FoodMassed()
					{{
						setName("Milk");
						setRelProts(2.8);
						setRelFats(3.2);
						setRelCarbs(4.7);
						setRelValue(58);
						setMass(120.5);
					}});
				}});
			}});
			add((Versioned) new Versioned<NoteRecord>()
			{{
				setId("4");
				setTimeStamp(new Date());
				setHash("hash");
				setVersion(13);
				setDeleted(true);
				setData(new NoteRecord()
				{{
					setTime(new Date());
					setText("It works :)");
				}});
			}});
		}};
	}
}
