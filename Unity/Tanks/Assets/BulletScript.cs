using System.Collections;
using System.Collections.Generic;
using UnityEngine;
public class BulletScript : MonoBehaviour {

	// Use this for initialization
	public GameObject brickExplodePrefab;
	public Transform brickExplodeSpawn;
	static GameObject mapGO;
	string collisionTag;
	void Start () {
		mapGO = GameObject.Find("Map");
	}
	
	// Update is called once per frame
	void Update () {

	}

	void OnCollisionEnter(Collision col)
	{
		collisionTag = col.gameObject.tag;
		if (collisionTag == "Brick")
		{
			Destroy(this.gameObject);
			col.gameObject.GetComponent<BrickScript>().Explode();
			

			// Destroy(col.gameObject);
		}
		else if (collisionTag == "Stone")
		{
			Destroy(this.gameObject);
		}
		else if (collisionTag == "EnemyTank")
		{
			EnemyTankScript enemyTankScript = col.gameObject.GetComponent<EnemyTankScript>();
			enemyTankScript.Explode();
			EnemyTankScript.setNextID(enemyTankScript.getID());
			mapGO.GetComponent<MapScript>().createEnemyTank();
			Destroy(this.gameObject);
		}
		else if(collisionTag == "Tank")
		{
			PlayerTankScript tankScript = col.gameObject.GetComponent<PlayerTankScript>();
			tankScript.gotHit();
		}
	}
}
