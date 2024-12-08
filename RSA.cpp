#include <iostream>
#include <iomanip>
#include <vector>
#include <random>
#include <assert.h>
#include <bitset> 

using namespace std;

class big_num {
	friend int compare(big_num& A, big_num& B);
	public:
		vector<unsigned int>data;
		
		big_num(){}
		big_num(unsigned int number) {
			data.push_back(number);
		}
		big_num(std::vector<unsigned int>::iterator it, int size) {
			for (int j = 0; j < size; j++) {
				data.push_back(*it);
				it++;
			}
		}

		big_num addition(big_num& operand_R) { 	// addition (operand_L(this) + operand_R)
			big_num result;
			unsigned int carry = 0;
			unsigned int temp;
			unsigned int L_val, R_val;
			int max_size = (int)this->data.size();
			if (max_size < (int)operand_R.data.size()) {
				max_size = (int)operand_R.data.size();
			}

			for (int index = 0; index < max_size; index++) {
				if (index < (int)this->data.size()) {
					L_val = this->data[index];
				}
				else {
					L_val = 0;
				}

				if (index < (int)operand_R.data.size()) {
					R_val = operand_R.data[index];
				}
				else {
					R_val = 0;
				}

				temp = L_val + R_val + carry;
			
				if ((temp >= L_val) && (temp >= R_val)) {
					carry = 0;
				}
				else {	// overflow
					carry = 1;
				}

				result.data.push_back(temp);
			}

			if (carry) {
				result.data.push_back(1);
			}
			return result;
		}

		big_num subtraction_abs(big_num& operand_R) {
			if (compare(*this, operand_R) == -1) {
				return (operand_R.sub_helper(*this));
			}
			return (this->sub_helper(operand_R));
		}

		big_num sub_helper(big_num& operand_R) {		// subtraction (operand_L(this) >= operand_R)
			big_num result;
			unsigned int borrow = 0;
			unsigned int temp;
			unsigned int L_val, R_val;
			int max_size = (int)this->data.size();

			for (int index = 0; index < max_size; index++) {
				if (index < (int)this->data.size()) {
					L_val = this->data[index];
				}
				else {
					L_val = 0;
				}

				if (index < (int)operand_R.data.size()) {
					R_val = operand_R.data[index];
				}
				else {
					R_val = 0;
				}

				temp = L_val - R_val - borrow;

				if (temp > L_val) {	// overflow
					borrow = 1;
				}
				else {
					borrow = 0;
				}

				result.data.push_back(temp);
			}

			for (int index = (int)(result.data.size() - 1); index > 0; index--) {
				if (!result.data[index]) {
					result.data.pop_back();
				}
				else {
					break;
				}
			}
			return result;
		}

		/*
		big_num sub_mod(big_num operand_R, big_num mod_num) {	// ((operand_L - operand_R) % mod_num) operand_L(this) < mod_num, operand_R < mod_num
			int cmp_result = compare(*this, operand_R);

			if (cmp_result == 0) {	// operand_L(this) == operand_R
				big_num result(0);
				return result;
			}
			if(cmp_result == 1){
				big_num result;
				result = this->mod(operand_R);
				return result;
			}
			big_num result;
			result = this->addition(mod_num);
			result = result.mod(mod_num);
			return result;
		}*/
		
		int highest_bit_index() {
			if ((this->data.size() == 1) && (!this->data[0])) {
				return -1;
			}
			unsigned int num_mask = 0x80000000;
			int highest_index = 31;
			int size = (int)this->data.size();
			unsigned int highest_unit = this->data[size - 1];

			while (1) {
				if (highest_unit & num_mask) {
					break;
				}
				highest_index--;
				num_mask = num_mask >> 1;
			}
			return ((size - 1) * 32 + highest_index);
		}

		big_num shift_left(int bits) {
			big_num result = *this; //big_num& result = new big_num(*this)
			int shift_units = bits / 32;
			int shift_bits = bits % 32;

			unsigned int shift_in = 0, shift_out = 0;
			for (int j = 0; j < result.data.size(); j++) {
				shift_out = result.data[j] >> (32 - bits);
				result.data[j] <<= bits;
				result.data[j] += shift_in;

				shift_in = shift_out;
			}

			if (shift_out) {
				result.data.push_back(shift_out);
			}

			for (int j = 0; j < shift_units; j++) {
				result.data.insert(result.data.begin(), 0);
			}

			return result;
		}

		big_num shift_right(int bits) {
			big_num result = *this; //big_num& result = new big_num(*this)
			int shift_units = bits / 32;
			int shift_bits = bits % 32;

			for (int j = 0; j < shift_units; j++) {
				result.data.erase(result.data.begin());
			}

			int size = (int)result.data.size();
			unsigned int shift_in = 0, shift_out = 0;
			for (int j = size - 1; j >= 0; j--) {
				shift_out = result.data[j] << (32 - bits);
				result.data[j] >>= bits;
				result.data[j] += shift_in;

				shift_in = shift_out;
			}

			if (!result.data[size - 1]) {
				result.data.pop_back();
			}

			return result;
		}

		int div(big_num& B, big_num& Q, big_num& R) {	// this / B = Q ... R. Q(0), R(0)
			int cmp_result = compare(*this, B);
			if (B.is_zero()) {
				return -1;
			}

			if (is_zero()) {
				Q.data.clear();
				R.data.clear();
				Q.data.push_back(0);
				R.data.push_back(0);
				return 0;
			}

			if (cmp_result == 0) {	// operand(this) == mod_num
				Q.data.clear();
				R.data.clear();
				Q.data.push_back(1);
				R.data.push_back(0);
				return 0;
			}

			if (cmp_result == -1) {	// operand(this) < mod_num
				Q.data.clear();
				Q.data.push_back(0);
				R = *this;
				return 0;
			}

			Q.data.clear();
			Q.data.push_back(0);
			int bit_difference = highest_bit_index() - B.highest_bit_index();
			big_num temp = B.shift_left(bit_difference);
			for (; bit_difference >= 0; bit_difference--) {
				Q = Q.shift_left(1);
				if (compare(*this, temp) != -1) {
					*this = subtraction_abs(temp);
					big_num one(1);
					Q = Q.addition(one);
				}
				temp = temp.shift_right(1);
			}
			
			R = *this;
			return 0;
		}

		bool is_zero() {
			if ((this->data.size() == 1) && (!this->data[0])) {
				return true;
			}
			return false;
		}

		big_num multiplication(big_num& operand_R) {		// multiplication (A * B)
			big_num result(0);
			if (this->is_zero() || operand_R.is_zero()) {
				return result;
			}

			big_num addend;
			for (int index_L = 0; index_L < (int)this->data.size(); index_L++) {
				for (int index_R = 0; index_R < (int)operand_R.data.size(); index_R++) {
					unsigned long long temp = (unsigned long long)this->data[index_L] * (unsigned long long)operand_R.data[index_R];

					for (int j = 0; j < index_L + index_R; j++) {
						addend.data.push_back(0);
					}
					addend.data.push_back((unsigned int)(temp & 0x00000000ffffffff));	// bit 0 ~ 31;
					addend.data.push_back((unsigned int)(temp >> 32));	// bit 32 ~ 63;
					
					result = result.addition(addend);

					addend.data.clear();
				}
			}

			for (int index = (int)(result.data.size() - 1); index > 0; index--) {
				if (!result.data[index]) {
					result.data.pop_back();
				}
				else {
					break;
				}
			}

			return result;
		}

		bool is_this_bit_1(int bit_index) {
			int which_unit = bit_index / 32;
			int which_bit = bit_index % 32;

			if (this->data[which_unit] & (1 << which_bit)) {
				return true;
			}
			return false;
		}

		big_num exp_mod(big_num& E, big_num& N) {	// (this^E) % N
			assert(!N.is_zero());
			big_num result(1);
			if (E.is_zero()) {
				return result;
			}

			int E_bits = E.highest_bit_index();
			big_num Q(0), R(0);
			for (int j = E_bits; j >= 0; j--) {
				result = result.multiplication(result);
				result.div(N, Q, R);
				result = R;
				if (E.is_this_bit_1(j)) {
					result = result.multiplication(*this);
					result.div(N, Q, R);
					result = R;
				}
			}

			return R;
		}

		void randomize(int bits, bool highest_bit_is_1, bool is_odd) {
			int units = bits / 32;
			int extra_bits = bits % 32;
			random_device rd;
			mt19937 gen(rd());
			uniform_int_distribution<unsigned int> dist(1, 0xffffffff);
			data.clear();
			for (int j = 0; j < units; j++) {
				data.push_back(dist(gen));
			}

			if (extra_bits) {
				data.push_back(dist(gen));
				data[(int)data.size() - 1] &= ((1 << extra_bits) - 1);
			}

			if (highest_bit_is_1) {
				if (extra_bits) {
					data[(int)data.size() - 1] |= (1 << (extra_bits - 1));
				}
				else {
					data[(int)data.size() - 1] |= (1 << 31);
				}
			}

			if (is_odd) {
				data[0] |= 1;
			}
		}

		void print_in_binary() {
			int bits = this->highest_bit_index() + 1;
			int units = bits / 32;
			int extra_bits = bits % 32;

			if (data.size()) {
				for (int j = (int)data.size() - 1; j >= 0; j--) {
					cout << bitset<32>(data[j]);
				}
				cout << endl;
			}
		}

		int lowest_1_index() {
			int index = 0;
			if ((is_zero()) || (!data.size())) {
				return -1;
			}

			big_num temp = *this;
			while (1) {
				if (temp.data[0] & 1) {
					return index;
				}
				temp = temp.shift_right(1);
				index++;
			}
		}

		bool miller_rabin() {	// Test if (*this) is prime
			assert(data.size());
			if ((data.size() == 1) && (data[0] == 2)) {
				return true;
			}
			if (((data[0] & 1) == 0)) {
				return false;
			}
			
			big_num Q(0), R(0);	// for div (mod)
			big_num one(1);
			big_num minus_one;
			minus_one = subtraction_abs(one);
			int S_orig = minus_one.lowest_1_index();
			int S = S_orig;
			big_num D = minus_one.shift_right(S_orig);
			for (int j = 0; j < 30; j++) {
				S = S_orig;
				big_num test_num;
				test_num.randomize(40, 0, 0);
				big_num orig = test_num;
				test_num = test_num.exp_mod(D, *this);	// test_num : Y

				while (S >= 0) {
					if (compare(one, test_num) == 0) {	// Y == 1
						break;
					}
					if (S > 0) {
						if (compare(minus_one, test_num) == 0) {	// Y == -1
							break;
						}
					}

					S--;
					test_num = test_num.multiplication(test_num);
					test_num.div(*this, Q, R);
					test_num = R;
				}

				if (S < 0) {
					return false;
				}
			}
			return true;
		}
};

int compare(big_num& operand_L, big_num& operand_R) {
	if (operand_L.data.size() < operand_R.data.size()) {
		return -1;
	}
	if (operand_L.data.size() > operand_R.data.size()) {
		return 1;
	}
	for (int index = (int)(operand_L.data.size() - 1); index >= 0; index--) {
		if (operand_L.data[index] < operand_R.data[index]) {
			return -1;
		}
		else if (operand_L.data[index] > operand_R.data[index]) {
			return 1;
		}
	}

	return 0;
}



int main() {
	big_num A(4001), B(4002), C(4003), D(4004), E(4005), F(4006), G(4007);
	
	cout << A.miller_rabin() << endl;	// T
	cout << B.miller_rabin() << endl;	// F
	cout << C.miller_rabin() << endl;	// T
	cout << D.miller_rabin() << endl;	// F
	cout << E.miller_rabin() << endl;	// F
	cout << F.miller_rabin() << endl;	// F
	cout << G.miller_rabin() << endl;	// T
	return 0;
}